{-# LANGUAGE OverloadedStrings #-}

module Sqint.PgAstSpec (tests) where

import Data.Set qualified as Set
import Data.Text qualified as T
import RIO
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.PgAst (classifySql)
import Test.Tasty
import Test.Tasty.HUnit


-- | Most tests don't exercise the CHECK-domain rule.
noDomains :: FilePath -> Text -> IO [Finding]
noDomains = classifySql mempty


withDomains :: [Text] -> FilePath -> Text -> IO [Finding]
withDomains domains = classifySql (Set.fromList domains)


tests :: TestTree
tests =
    testGroup
        "PgAst"
        [ testGroup
            "empty / trivial"
            [ testCase "empty source yields no findings" $ do
                findings <- noDomains "empty.sql" ""
                findings @?= []
            , testCase "plain SELECT yields no findings" $ do
                findings <- noDomains "select.sql" "select 1;"
                findings @?= []
            , testCase "CREATE TABLE is not a lock concern" $ do
                findings <- noDomains "create.sql" "create table foo (id int primary key);"
                findings @?= []
            ]
        , testGroup
            "ADD COLUMN"
            [ testCase "ADD COLUMN NOT NULL without DEFAULT blocks (PMA-1)" $ do
                let sql = "alter table foo add column created_at timestamptz not null;"
                findings <- noDomains "add-not-null.sql" sql
                map (.severity) findings @?= [SeverityBlock]
            , testCase "ADD COLUMN NOT NULL with DEFAULT is info (catalog-only)" $ do
                let sql = "alter table foo add column created_at timestamptz not null default now();"
                findings <- noDomains "add-with-default.sql" sql
                map (.severity) findings @?= [SeverityInfo]
            , testCase "ADD COLUMN nullable is info (catalog-only)" $ do
                let sql = "alter table foo add column note text;"
                findings <- noDomains "add-nullable.sql" sql
                map (.severity) findings @?= [SeverityInfo]
            , testCase "ADD COLUMN of a CHECK-bearing domain blocks (R6)" $ do
                let sql = "alter table foo add column code ssn;"
                findings <- withDomains ["ssn"] "add-domain.sql" sql
                map (.severity) findings @?= [SeverityBlock]
            , testCase "ADD COLUMN of a non-CHECK domain does not upgrade severity" $ do
                let sql = "alter table foo add column code ssn;"
                findings <- withDomains [] "add-plain-domain.sql" sql
                map (.severity) findings @?= [SeverityInfo]
            ]
        , testGroup
            "ADD CONSTRAINT"
            [ testCase "ADD CHECK without NOT VALID blocks" $ do
                let sql = "alter table foo add constraint c check (x > 0);"
                findings <- noDomains "add-check.sql" sql
                map (.severity) findings @?= [SeverityBlock]
            , testCase "ADD CHECK ... NOT VALID is info (catalog-only)" $ do
                let sql = "alter table foo add constraint c check (x > 0) not valid;"
                findings <- noDomains "add-check-not-valid.sql" sql
                map (.severity) findings @?= [SeverityInfo]
            , testCase "ADD FOREIGN KEY without NOT VALID blocks" $ do
                let sql =
                        "alter table orders add constraint fk_customer "
                            <> "foreign key (customer_id) references customers (id);"
                findings <- noDomains "add-fk.sql" sql
                map (.severity) findings @?= [SeverityBlock]
            , testCase "ADD FOREIGN KEY ... NOT VALID is info (catalog-only)" $ do
                let sql =
                        "alter table orders add constraint fk_customer "
                            <> "foreign key (customer_id) references customers (id) not valid;"
                findings <- noDomains "add-fk-not-valid.sql" sql
                map (.severity) findings @?= [SeverityInfo]
            ]
        , testGroup
            "ALTER COLUMN TYPE"
            [ testCase "ALTER COLUMN TYPE blocks" $ do
                let sql = "alter table foo alter column note type text;"
                findings <- noDomains "alter-type.sql" sql
                map (.severity) findings @?= [SeverityBlock]
            ]
        , testGroup
            "CREATE INDEX"
            [ testCase "CREATE INDEX (non-concurrent) warns" $ do
                findings <- noDomains "create-index.sql" "create index foo_idx on foo (x);"
                map (.severity) findings @?= [SeverityWarn]
            , testCase "CREATE INDEX CONCURRENTLY is allowed" $ do
                let sql = "create index concurrently foo_idx on foo (x);"
                findings <- noDomains "create-index-concurrent.sql" sql
                map (.severity) findings @?= [SeverityInfo]
            ]
        , testGroup
            "DROP INDEX"
            [ testCase "DROP INDEX (non-concurrent) is info (brief catalog-only lock)" $ do
                findings <- noDomains "drop-index.sql" "drop index foo_idx;"
                map (.severity) findings @?= [SeverityInfo]
            , testCase "DROP INDEX CONCURRENTLY is allowed" $ do
                findings <- noDomains "drop-index-concurrent.sql" "drop index concurrently foo_idx;"
                map (.severity) findings @?= [SeverityInfo]
            ]
        , testGroup
            "ALTER DOMAIN"
            [ testCase "ALTER DOMAIN ADD CONSTRAINT without NOT VALID blocks" $ do
                let sql = "alter domain ssn add constraint c check (length(value) = 11);"
                findings <- noDomains "alter-domain.sql" sql
                map (.severity) findings @?= [SeverityBlock]
            , testCase "ALTER DOMAIN ADD CONSTRAINT ... NOT VALID is info (catalog-only)" $ do
                let sql = "alter domain ssn add constraint c check (length(value) = 11) not valid;"
                findings <- noDomains "alter-domain-not-valid.sql" sql
                map (.severity) findings @?= [SeverityInfo]
            ]
        , testGroup
            "Source range tracking"
            [ testCase "first-statement range starts at line 1" $ do
                let sql = "create index foo_idx on foo (x);"
                findings <- noDomains "ranges.sql" sql
                map (\f -> f.range.startLine) findings @?= [1]
            , testCase "a later statement spans its own lines, not the gap before it" $ do
                -- libpg_query starts every statement but the first just past
                -- the previous semicolon, so the header, the blank lines and
                -- the comment here all belong to the index's raw span.
                let sql =
                        T.unlines
                            [ "-- Deploy app:widgets to pg" -- 1
                            , "" -- 2
                            , "begin;" -- 3
                            , "" -- 4
                            , "-- for lookups by name" -- 5
                            , "create index widgets_name" -- 6
                            , "    on widgets (name);" -- 7
                            , "" -- 8
                            , "commit;" -- 9
                            ]
                findings <- noDomains "ranges.sql" sql
                map (\f -> (f.range.startLine, f.range.endLine)) findings @?= [(6, 7)]
            ]
        ]
