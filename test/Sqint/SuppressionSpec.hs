module Sqint.SuppressionSpec (tests) where

import Data.Text qualified as T
import RIO
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.PgAst (classifySql)
import Sqint.Rule (Rule (..), renderRule)
import Sqint.Suppression (Annotation (..), applyAnnotations, parseAnnotations)
import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests =
    testGroup
        "Suppression"
        [ testCase "standalone annotation targets next non-blank line" do
            let src =
                    """
                    -- lint:ignore reason="empty table"

                    create index on foo(x);
                    """
            let (annotations, _) = parseAnnotations "file.sql" src
            map (\a -> a.line) annotations @?= [3]
        , testCase "trailing annotation targets its own line" do
            let src = T.unlines ["create index on foo(x); -- lint:ignore reason=\"empty\""]
            let (annotations, _) = parseAnnotations "file.sql" src
            map (\a -> a.line) annotations @?= [1]
        , testCase "reason is extracted" do
            let src = "-- lint:ignore reason=\"because reasons\"\ncreate index on foo(x);\n"
            let (annotations, _) = parseAnnotations "file.sql" src
            map (\a -> a.reason) annotations @?= ["because reasons"]
        , testCase "annotation without reason yields a warn finding" do
            let src =
                    """
                    -- lint:ignore
                    create index on foo(x);
                    """
            let (annotations, malformed) = parseAnnotations "file.sql" src
            length annotations @?= 0
            map (\f -> f.severity) malformed @?= [SeverityWarn]
            map (\f -> f.range.startLine) malformed @?= [1]
            map (\f -> renderRule f.rule) malformed @?= ["suppression/missing-reason"]
        , testCase "annotation with empty reason yields a warn finding" do
            let src =
                    """
                    -- lint:ignore reason=""
                    create index on foo(x);
                    """
            let (annotations, malformed) = parseAnnotations "file.sql" src
            length annotations @?= 0
            map (\f -> renderRule f.rule) malformed @?= ["suppression/missing-reason"]
        , testCase "matching annotation marks finding suppressed" do
            let f =
                    Finding
                        { range = SourceRange "file.sql" 3 1 3 30
                        , severity = SeverityBlock
                        , rule = LockAddConstraint
                        , title = "AccessExclusive + TableScan"
                        , detail = ""
                        , suppressedBy = Nothing
                        }
            let annotations = [Annotation{line = 3, reason = "because"}]
            map (\g -> g.suppressedBy) (applyAnnotations annotations [f]) @?= [Just "because"]
        , testCase "non-matching annotation leaves finding active" do
            let f =
                    Finding
                        { range = SourceRange "file.sql" 3 1 3 30
                        , severity = SeverityBlock
                        , rule = LockAddColumn
                        , title = "t"
                        , detail = ""
                        , suppressedBy = Nothing
                        }
            let annotations = [Annotation{line = 7, reason = "wrong"}]
            map (\g -> g.suppressedBy) (applyAnnotations annotations [f]) @?= [Nothing]
        , testCase "each annotation silences the statement it is written against" do
            -- Above a statement, or trailing any of its lines; never the
            -- statement after it.
            let src =
                    T.unlines
                        [ "-- Deploy app:widgets to pg" -- 1
                        , "" -- 2
                        , "begin;" -- 3
                        , "" -- 4
                        , "alter table a add column x int not null; -- lint:ignore reason=\"trailing\"" -- 5
                        , "" -- 6
                        , "-- lint:ignore reason=\"above\"" -- 7
                        , "create index a_x" -- 8
                        , "    on a (x);" -- 9
                        , "" -- 10
                        , "create index b_x" -- 11
                        , "    on b (x); -- lint:ignore reason=\"on its last line\"" -- 12
                        , "" -- 13
                        , "create index c_x on c (x);" -- 14
                        , "commit;" -- 15
                        ]
            findings <- classifySql mempty "file.sql" src
            let (annotations, _) = parseAnnotations "file.sql" src
            [(f.range.startLine, f.suppressedBy) | f <- applyAnnotations annotations findings]
                @?= [ (5, Just "trailing")
                    , (8, Just "above")
                    , (11, Just "on its last line")
                    , (14, Nothing)
                    ]
        ]
