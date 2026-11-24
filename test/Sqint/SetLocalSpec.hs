{-# LANGUAGE OverloadedStrings #-}

module Sqint.SetLocalSpec (tests) where

import Data.String.Interpolate (i)
import RIO
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.Rule (renderRule)
import Sqint.SetLocal (detectSetLocalAcrossCommit)
import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests =
    testGroup
        "SetLocal"
        [ testCase "empty source yields no findings" $ do
            findings <- detectSetLocalAcrossCommit "empty.sql" ""
            findings @?= []
        , testCase "SET LOCAL in a DO block that COMMITs is blocked" $ do
            findings <- detectSetLocalAcrossCommit "b.sql" batchingDoBlock
            map (.severity) findings @?= [SeverityBlock]
            map (renderRule . (.rule)) findings @?= ["txn/set-local-across-commit"]
        , testCase "set_config(.., true) in a block that COMMITs is blocked" $ do
            findings <- detectSetLocalAcrossCommit "b.sql" (batchingWith "perform set_config('lock_timeout', '50ms', true);")
            map (.severity) findings @?= [SeverityBlock]
        , -- The correct shape (bulk_update.tmpl): session scope survives COMMIT.
          testCase "set_config(.., false) in a block that COMMITs is fine" $ do
            findings <- detectSetLocalAcrossCommit "b.sql" (batchingWith "perform set_config('lock_timeout', '50ms', false);")
            findings @?= []
        , -- Without a COMMIT the transaction scope is the whole block.
          testCase "SET LOCAL with no COMMIT anywhere is fine" $ do
            let body =
                    """
                    do $$
                    begin
                        set local lock_timeout to '50ms';
                        update orders set y = 1;
                    end $$;
                    """
            findings <- detectSetLocalAcrossCommit "b.sql" body
            findings @?= []
        , testCase "ROLLBACK ends the transaction just as effectively" $ do
            let body =
                    """
                    do $$
                    begin
                        set local lock_timeout to '50ms';
                        loop
                            update orders set y = 1;
                            rollback;
                        end loop;
                    end $$;
                    """
            findings <- detectSetLocalAcrossCommit "b.sql" body
            map (.severity) findings @?= [SeverityBlock]
        , testCase "a plpgsql function body is checked, not just DO blocks" $ do
            let body =
                    """
                    create or replace procedure p() as
                    $$
                    begin
                        set local lock_timeout to '50ms';
                        loop
                            update orders set y = 1;
                            commit;
                        end loop;
                    end
                    $$ language plpgsql;
                    """
            findings <- detectSetLocalAcrossCommit "p.sql" body
            map (.severity) findings @?= [SeverityBlock]
        , -- Formatting must not smuggle a transaction-scoped set_config past the match.
          testCase "set_config spread over several lines is still matched" $ do
            let body =
                    """
                    do $$
                    begin
                        perform set_config('lock_timeout',
                                           '50ms',
                                           true);
                        loop
                            update orders set y = 1;
                            commit;
                        end loop;
                    end $$;
                    """
            findings <- detectSetLocalAcrossCommit "b.sql" body
            map (.severity) findings @?= [SeverityBlock]
        , testCase "a SQL-language function is not inspected" $ do
            let body =
                    """
                    create or replace function f() returns int as
                    $$ select 1 $$ language sql;
                    """
            findings <- detectSetLocalAcrossCommit "f.sql" body
            findings @?= []
        , -- Unparseable bodies produce no findings rather than aborting the linter.
          testCase "unparseable source yields no findings" $ do
            findings <- detectSetLocalAcrossCommit "junk.sql" "this is not sql at all ("
            findings @?= []
        , testCase "both settings in one block are reported separately" $ do
            let body =
                    """
                    do $$
                    begin
                        set local lock_timeout to '50ms';
                        perform set_config('synchronous_commit', 'remote_apply', true);
                        loop
                            update orders set y = 1;
                            commit;
                        end loop;
                    end $$;
                    """
            findings <- detectSetLocalAcrossCommit "b.sql" body
            length findings @?= 2
        , testCase "the finding points at the offending line, not the block" $ do
            findings <- detectSetLocalAcrossCommit "b.sql" batchingDoBlock
            map ((.startLine) . (.range)) findings @?= [3]
        ]


-- | A transaction-scoped setting above a loop that COMMITs per batch.
batchingDoBlock :: Text
batchingDoBlock = batchingWith "set local lock_timeout to '50ms';"


-- | A batching DO block whose first statement is the given one.
batchingWith :: Text -> Text
batchingWith setting =
    [i|do $$
begin
    #{setting}
    loop
        update orders set y = 1 where x = 2;
        exit when not found;
        commit;
    end loop;
end $$;
|]
