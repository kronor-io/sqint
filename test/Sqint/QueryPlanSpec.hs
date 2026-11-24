{-# LANGUAGE OverloadedStrings #-}

module Sqint.QueryPlanSpec (tests) where

import RIO
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.QueryPlan (detectIndexedCasts)
import Sqint.Rule (renderRule)
import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests =
    testGroup
        "QueryPlan"
        [ testCase "empty source yields no findings" $ do
            findings <- detectIndexedCasts "empty.sql" ""
            findings @?= []
        , testCase "plain WHERE on a column does not warn" $ do
            findings <- detectIndexedCasts "p.sql" "select * from orders where id = 5;"
            findings @?= []
        , testCase "WHERE id::text = ... warns (PMA-2 shape)" $ do
            findings <- detectIndexedCasts "p.sql" "select * from orders where id::text = 'foo';"
            map (.severity) findings @?= [SeverityWarn]
        , testCase "UPDATE ... WHERE id::text = ... warns" $ do
            findings <- detectIndexedCasts "p.sql" "update orders set y = 1 where id::text = 'foo';"
            map (.severity) findings @?= [SeverityWarn]
        , testCase "DELETE ... WHERE id::text = ... warns" $ do
            findings <- detectIndexedCasts "p.sql" "delete from orders where id::text = 'foo';"
            map (.severity) findings @?= [SeverityWarn]
        , testCase "cast on a literal does not warn" $ do
            findings <- detectIndexedCasts "p.sql" "select * from orders where id = '5'::int;"
            findings @?= []
        , testCase "nested boolean: AND with column cast warns" $ do
            findings <- detectIndexedCasts "p.sql" "select * from orders where status = 'paid' and id::text = 'foo';"
            map (.severity) findings @?= [SeverityWarn]
        , testCase "JOIN ... ON a.id::text = b.ref warns (ON predicate context)" $ do
            findings <- detectIndexedCasts "p.sql" "select * from a join b on a.id::text = b.ref;"
            map (.severity) findings @?= [SeverityWarn]
        , testCase "WHERE id::text IN (...) warns" $ do
            findings <- detectIndexedCasts "p.sql" "select * from orders where id::text in ('a', 'b');"
            map (.severity) findings @?= [SeverityWarn]
        , testCase "WHERE id::text NOT IN (...) warns" $ do
            findings <- detectIndexedCasts "p.sql" "select * from orders where id::text not in ('a', 'b');"
            map (.severity) findings @?= [SeverityWarn]
        , testCase "cast in the SELECT projection does not warn" $ do
            findings <- detectIndexedCasts "p.sql" "select id::text from orders;"
            findings @?= []
        , testCase "cast in a CTE projection does not warn" $ do
            findings <- detectIndexedCasts "p.sql" "with c as (select id::text as i from orders) select * from c;"
            findings @?= []
        , testCase "filtering on a CTE's cast projection output does not warn" $ do
            findings <- detectIndexedCasts "p.sql" "with c as (select id::text as i from orders) select * from c where i = 'x';"
            findings @?= []
        , testCase "comparison in the projection (not a scan qual) does not warn" $ do
            findings <- detectIndexedCasts "p.sql" "select id::text = 'x' as m from orders;"
            findings @?= []
        , testCase "cast in a RETURNING list does not warn" $ do
            findings <- detectIndexedCasts "p.sql" "update orders set y = 1 where x = 2 returning id::text;"
            findings @?= []
        , testCase "cast in an UPDATE SET assignment does not warn" $ do
            findings <- detectIndexedCasts "p.sql" "update orders set ref = id::text where x = 2;"
            findings @?= []
        , testCase "cast in HAVING does not warn (post-aggregation, off the scan path)" $ do
            findings <- detectIndexedCasts "p.sql" "select m from orders group by m having m::text = 'x';"
            findings @?= []
        , testCase "aggregate in HAVING does not warn" $ do
            findings <- detectIndexedCasts "p.sql" "select m from orders group by m having count(*) > 5;"
            findings @?= []
        , testCase "WHERE lower(id) = ... warns as index/function-on-column" $ do
            findings <- detectIndexedCasts "p.sql" "select * from orders where lower(id) = 'foo';"
            map (.severity) findings @?= [SeverityWarn]
            map (renderRule . (.rule)) findings @?= ["index/function-on-column"]
        , testCase "WHERE date_trunc on a column warns" $ do
            findings <- detectIndexedCasts "p.sql" "select * from orders where date_trunc('day', created_at) = '2026-01-01';"
            map (.severity) findings @?= [SeverityWarn]
        , testCase "JOIN ... ON lower(a.id) = b.k warns" $ do
            findings <- detectIndexedCasts "p.sql" "select * from a join b on lower(a.id) = b.k;"
            map (renderRule . (.rule)) findings @?= ["index/function-on-column"]
        , testCase "function on a constant does not warn" $ do
            findings <- detectIndexedCasts "p.sql" "select * from orders where created_at > now();"
            findings @?= []
        , testCase "function call in the SELECT projection does not warn" $ do
            findings <- detectIndexedCasts "p.sql" "select lower(id) from orders;"
            findings @?= []
        , testCase "cast in a partial-index predicate warns" $ do
            findings <- detectIndexedCasts "p.sql" "create index on orders (m) where id::text = 'x';"
            map (.severity) findings @?= [SeverityWarn]
        , testCase "cast in a subquery's WHERE warns" $ do
            findings <- detectIndexedCasts "p.sql" "select * from a where x = (select count(*) from b where b.id::text = 'k');"
            map (.severity) findings @?= [SeverityWarn]
        , testCase "cast in a MERGE ... ON predicate warns (generic walk descends MERGE)" $ do
            findings <- detectIndexedCasts "p.sql" "merge into t using s on t.id::text = s.k when matched then update set a = 1;"
            map (.severity) findings @?= [SeverityWarn]
        , testCase "rule id is index/cast-on-column" $ do
            findings <- detectIndexedCasts "p.sql" "select * from orders where id::text = 'foo';"
            map (renderRule . (.rule)) findings @?= ["index/cast-on-column"]
        , testCase "cast inside a plpgsql trigger body warns at the body line" $ do
            let body =
                    """
                    create or replace function f() returns trigger as
                    $$
                    begin
                      update shop.checkout_session
                      set tally = tally + 1
                      where customer_id = new.customer_id
                          and id::text = new.order_reference;
                      return new;
                    end
                    $$ language plpgsql;
                    """
            findings <- detectIndexedCasts "trigger.sql" body
            map (.severity) findings @?= [SeverityWarn]
            map ((.startLine) . (.range)) findings @?= [4]
        , testCase "cast inside an IF/THEN inside plpgsql still warns" $ do
            let body =
                    """
                    create or replace function f() returns void as
                    $$
                    begin
                      if true then
                        update orders set y = 1 where id::text = 'foo';
                      end if;
                    end
                    $$ language plpgsql;
                    """
            findings <- detectIndexedCasts "guard.sql" body
            map (.severity) findings @?= [SeverityWarn]
        , testCase "no warn when plpgsql body has no column-ref cast" $ do
            let body =
                    """
                    create or replace function f() returns trigger as
                    $$
                    begin
                      update orders set y = 1 where id = new.id;
                      return new;
                    end
                    $$ language plpgsql;
                    """
            findings <- detectIndexedCasts "clean.sql" body
            findings @?= []
        , testCase "cast inside a BEGIN ATOMIC function body warns" $ do
            let body =
                    """
                    create or replace function f(p text) returns setof orders
                      begin atomic
                        select * from orders where id::text = p;
                      end;
                    """
            findings <- detectIndexedCasts "atomic.sql" body
            map (.severity) findings @?= [SeverityWarn]
        , testCase "dollar-tagged plpgsql body still works" $ do
            let body =
                    """
                    create or replace function f() returns trigger as
                    $func$
                    begin
                      update orders set y = 1 where id::text = 'foo';
                      return new;
                    end
                    $func$ language plpgsql;
                    """
            findings <- detectIndexedCasts "tagged.sql" body
            map (.severity) findings @?= [SeverityWarn]
        ]
