{-# LANGUAGE OverloadedStrings #-}

module Sqint.DeferredDependencySpec (tests) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import RIO
import Sqint.DeferredDependency (detectUnfinishedDependency, registrationsIn)
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.Rule (renderRule)
import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests =
    testGroup
        "DeferredDependency"
        [ -- Nothing registered means nothing to depend on, so the classifier
          -- returns before parsing.
          testCase "no registered backfills yields no findings" $ do
            findings <- detectUnfinishedDependency Map.empty "v.sql" validateOrders
            findings @?= []
        , testCase "VALIDATE CONSTRAINT on a table with an open backfill is blocked" $ do
            findings <- detectUnfinishedDependency ordersRegistered "v.sql" validateOrders
            map (.severity) findings @?= [SeverityBlock]
            map (renderRule . (.rule)) findings @?= ["deferred/unfinished-backfill"]
        , -- The usual way to finish a backfill: the same change both
          -- finishes the backfill and validates.
          testCase "calling ensure_finished in the same change satisfies it" $ do
            let src =
                    """
                    call deferred.ensure_finished('orders_status_backfill', 'my_change');
                    alter table shop.orders validate constraint orders_status_fkey;
                    """
            findings <- detectUnfinishedDependency ordersRegistered "f.sql" src
            findings @?= []
        , testCase "SET NOT NULL also requires the data to conform" $ do
            let src = "alter table shop.orders alter column status set not null;"
            findings <- detectUnfinishedDependency ordersRegistered "n.sql" src
            map (.severity) findings @?= [SeverityBlock]
        , -- A different table's backfill is not this change's problem.
          testCase "a table with no registered backfill is fine" $ do
            let src = "alter table shop.refund validate constraint refund_fkey;"
            findings <- detectUnfinishedDependency ordersRegistered "r.sql" src
            findings @?= []
        , testCase "an unqualified table name is treated as public" $ do
            let src = "alter table thing validate constraint thing_fkey;"
            findings <- detectUnfinishedDependency (Map.singleton "public.thing" (Set.singleton "public_thing_done_backfill")) "t.sql" src
            map (.severity) findings @?= [SeverityBlock]
        , -- Only the two forms that read every row matter; ordinary DDL on the
          -- same table is unaffected by an in-flight backfill.
          testCase "ADD COLUMN on the backfilled table is fine" $ do
            let src = "alter table shop.orders add column note text;"
            findings <- detectUnfinishedDependency ordersRegistered "a.sql" src
            findings @?= []
        , testCase "adding a NOT VALID constraint is fine" $ do
            let src =
                    """
                    alter table shop.orders
                        add constraint orders_status_fkey
                        foreign key (status) references shop.order_status (name)
                        not valid;
                    """
            findings <- detectUnfinishedDependency ordersRegistered "a.sql" src
            findings @?= []
        , testCase "empty source yields no findings" $ do
            findings <- detectUnfinishedDependency ordersRegistered "e.sql" ""
            findings @?= []
        , testCase "unparseable source yields no findings" $ do
            findings <- detectUnfinishedDependency ordersRegistered "junk.sql" "not sql at all ("
            findings @?= []
        , testCase "two dependent statements are reported separately" $ do
            let src =
                    """
                    alter table shop.orders validate constraint orders_status_fkey;
                    alter table shop.orders alter column status set not null;
                    """
            findings <- detectUnfinishedDependency ordersRegistered "two.sql" src
            length findings @?= 2
        , -- Regresses if lineAt stops skipping the leading whitespace libpg_query
          -- leaves before a second statement.
          testCase "the finding points at the offending statement's line" $ do
            let src =
                    """
                    alter table shop.orders add column note text;
                    alter table shop.orders validate constraint orders_status_fkey;
                    """
            findings <- detectUnfinishedDependency ordersRegistered "l.sql" src
            map ((.startLine) . (.range)) findings @?= [2]
        , -- One table can carry a backfill per column; all must be finished.
          testCase "every backfill open on the table is reported" $ do
            findings <- detectUnfinishedDependency twoOnOrders "v.sql" validateOrders
            length findings @?= 2
        , testCase "finalising one of two backfills still leaves the other" $ do
            let src =
                    """
                    call deferred.ensure_finished('orders_status_backfill', 'my_change');
                    alter table shop.orders validate constraint orders_status_fkey;
                    """
            findings <- detectUnfinishedDependency twoOnOrders "f.sql" src
            map (.severity) findings @?= [SeverityBlock]
        , -- Cases a whole-file string co-occurrence check would wrongly wave through.
          testCase "a comment mentioning ensure_finished does not satisfy it" $ do
            let src =
                    """
                    -- TODO: call deferred.ensure_finished('orders_status_backfill', 'later')
                    alter table shop.orders validate constraint orders_status_fkey;
                    """
            findings <- detectUnfinishedDependency ordersRegistered "c.sql" src
            map (.severity) findings @?= [SeverityBlock]
        , -- Substring matching cannot tell these apart; a literal comparison can.
          testCase "finalising a backfill whose name contains this one's does not satisfy it" $ do
            let src =
                    """
                    call deferred.ensure_finished('orders_status_backfill_v2', 'my_change');
                    alter table shop.orders validate constraint orders_status_fkey;
                    """
            findings <- detectUnfinishedDependency ordersRegistered "s.sql" src
            map (.severity) findings @?= [SeverityBlock]
        , -- Must not become so strict it misses a correct finaliser written differently.
          testCase "named notation satisfies it" $ do
            let src =
                    """
                    call deferred.ensure_finished(
                        migration_name := 'orders_status_backfill',
                        finalized_by_change := 'my_change');
                    alter table shop.orders validate constraint orders_status_fkey;
                    """
            findings <- detectUnfinishedDependency ordersRegistered "named.sql" src
            findings @?= []
        , testCase "an unqualified call satisfies it" $ do
            let src =
                    """
                    call ensure_finished('orders_status_backfill', 'my_change');
                    alter table shop.orders validate constraint orders_status_fkey;
                    """
            findings <- detectUnfinishedDependency ordersRegistered "bare.sql" src
            findings @?= []
        , -- The table comes from source_table, not from parsing the name.
          testCase "a registering insert is discovered with its table" $ do
            let src =
                    """
                    insert into deferred.migration
                        (name, enqueued_by, source_table, chunk_function,
                         key_column, key_type, target_batch_ms, max_batch_size)
                    values
                        ( 'orders_status_backfill'
                        , 'my_change'
                        , 'shop.orders'
                        , 'shop.backfill_orders_status'
                        , 'id'
                        , 'bigint'
                        , 2000
                        , 100000
                        )
                    on conflict (name) do nothing;
                    """
            registrationsIn src @?= ordersRegistered
        , -- A hand-written registration may list columns in any order.
          testCase "the column order is not assumed" $ do
            let src =
                    """
                    insert into deferred.migration
                        (source_table, key_type, name, enqueued_by, chunk_function, key_column)
                    values
                        ( 'shop.orders'
                        , 'bigint'
                        , 'orders_status_backfill'
                        , 'my_change'
                        , 'shop.backfill_orders_status'
                        , 'id'
                        );
                    """
            registrationsIn src @?= ordersRegistered
        , testCase "two registering inserts in one file are both discovered" $ do
            let src =
                    """
                    insert into deferred.migration
                        (name, enqueued_by, source_table, chunk_function, key_column, key_type)
                    values
                        ( 'orders_status_backfill', 'a', 'shop.orders'
                        , 'shop.backfill_orders_status', 'id', 'bigint' );
                    insert into deferred.migration
                        (name, enqueued_by, source_table, chunk_function, key_column, key_type)
                    values
                        ( 'orders_channel_backfill', 'b', 'shop.orders'
                        , 'shop.backfill_orders_channel', 'id', 'bigint' );
                    """
            registrationsIn src @?= twoOnOrders
        , -- Keywords are matched case-insensitively.
          testCase "an uppercased insert is still discovered" $ do
            let src =
                    """
                    INSERT INTO deferred.migration
                        (name, enqueued_by, source_table, chunk_function, key_column, key_type)
                    VALUES
                        ( 'orders_status_backfill', 'my_change', 'shop.orders'
                        , 'shop.backfill_orders_status', 'id', 'bigint' );
                    """
            registrationsIn src @?= ordersRegistered
        , testCase "a file with no registering insert registers nothing" $ do
            registrationsIn "alter table shop.orders add column note text;" @?= Map.empty
        , -- Only the insert creates a backfill.
          testCase "an update to the registry is not a registration" $ do
            registrationsIn "update deferred.migration set target_batch_ms = 4000 where name = 'x';"
                @?= Map.empty
        ]


ordersRegistered :: Map Text (Set Text)
ordersRegistered =
    Map.singleton "shop.orders" (Set.singleton "orders_status_backfill")


twoOnOrders :: Map Text (Set Text)
twoOnOrders =
    Map.singleton
        "shop.orders"
        (Set.fromList ["orders_status_backfill", "orders_channel_backfill"])


validateOrders :: Text
validateOrders = "alter table shop.orders validate constraint orders_status_fkey;"
