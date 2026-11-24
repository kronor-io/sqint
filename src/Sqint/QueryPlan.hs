{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Query-plan rule: detect non-sargable column references inside
-- scan-qualifying predicates.
--
-- Background: a trigger that compared @id::text = reference@ turned an
-- index lookup into a sequential scan on every write. Wrapping an indexed
-- column in an expression — a cast (@id::text@) or a function call
-- (@lower(id)@, @date_trunc('day', created_at)@) — makes Postgres treat
-- the operand as an expression, so the column's plain index is no longer
-- usable and the planner falls back to a sequential scan.
--
-- The danger is specific to /predicates/. Wrapping a column only defeats
-- an index when the result gates a scan or a join. The same expression
-- in a SELECT list, a CTE projection, a RETURNING list, an UPDATE SET
-- assignment, or a HAVING clause (which filters /after/ grouping, off the
-- scan path) is computed per output row and never influences the access
-- path, so those must not warn.
--
-- Implementation: 'detectIndexedCasts' descends to the scan/join
-- qualifier subtrees — WHERE, JOIN ON, MERGE ON\/WHEN, and partial-index
-- \/ ON CONFLICT predicates — using the generic "Sqint.NodeWalk"
-- traversal to reach those clauses no matter how deeply they are nested
-- (subqueries, CTEs, MERGE, …). Within each qualifier it looks for a
-- comparison 'Pg.A_Expr' whose direct operand is an 'IndexHazard': a
-- @col::type@ cast (WARN, @index/cast-on-column@) or a function call taking a
-- column argument (WARN, @index/function-on-column@). DML embedded in a PL/pgSQL
-- body is surfaced via "Sqint.Plpgsql", re-parsed, and run
-- through the same collector.
--
-- The rule has no schema or plan knowledge, so it cannot tell an indexed
-- column from an unindexed one; reviewers can add an inline
-- @-- lint:ignore reason="…"@ when the column is not indexed. Only direct
-- comparison operands are inspected, so an expression wrapped one level
-- deeper (@lower(id) || 'x' = …@) and the @CASE@\/@COALESCE@ expression
-- nodes are intentionally out of scope for now.
module Sqint.QueryPlan (
    detectIndexedCasts,
) where

import Data.Text qualified as T
import PgQuery qualified as Pg
import RIO
import RIO.Set qualified as Set
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.Location (sourceRangeAt)
import Sqint.NodeWalk (universeOfType)
import Sqint.Plpgsql (EmbeddedSql (..), extractEmbeddedSql)
import Sqint.Rule (Rule (..))


-- | The two index-defeating operand shapes this rule recognises. Each
-- maps to its own rule id so reviewers can suppress them independently.
data IndexHazard
    = -- | @col::type@ — a cast of a bare column reference.
      CastHazard
    | -- | @f(col, …)@ — a function call that takes a column as an argument.
      FuncHazard
    deriving (Eq, Ord)


detectIndexedCasts ::
    FilePath ->
    Text ->
    IO [Finding]
detectIndexedCasts path src = do
    topLevel <- topLevelFindings path src
    embedded <- embeddedFindings path src
    pure (topLevel <> embedded)


-- | Hazards in the regular protobuf parse tree. PL/pgSQL function bodies
-- are handled separately in 'embeddedFindings'.
topLevelFindings :: FilePath -> Text -> IO [Finding]
topLevelFindings path src = do
    result <- Pg.parseSql (T.unpack src)
    pure $ case result of
        Left _ -> []
        Right parsed ->
            [ hazardFinding hz (sourceRangeAt path src loc)
            | (hz, loc) <- predicateHazards parsed
            ]


-- | Hazards inside PL/pgSQL function bodies. We extract every embedded
-- SQL statement via the plpgsql parser, re-parse each through
-- 'PgQuery.parseSql', and run the same collector. Findings are located
-- at the embedded statement's outer-file line (column 1) — column-level
-- precision isn't worth the bookkeeping for a rule whose primary value
-- is "look here" — so we emit one per distinct hazard kind present.
embeddedFindings :: FilePath -> Text -> IO [Finding]
embeddedFindings path src = do
    embeds <- extractEmbeddedSql src
    concat <$> traverse (embedFindings path) embeds


embedFindings :: FilePath -> EmbeddedSql -> IO [Finding]
embedFindings path e = do
    result <- Pg.parseSql (T.unpack e.sql)
    pure $ case result of
        Left _ -> []
        Right parsed ->
            [ hazardFinding hz (embeddedRange path e.fileLine)
            | hz <- Set.toList (Set.fromList (map fst (predicateHazards parsed)))
            ]


-- | Every index hazard that appears as a direct operand of a comparison
-- inside a scan\/join qualifier, paired with its source location.
-- Deduplicated because nested qualifiers (a subquery's WHERE inside an
-- outer WHERE) are reached through more than one root.
predicateHazards :: Pg.ParseResult -> [(IndexHazard, Int32)]
predicateHazards parsed =
    Set.toList
        $ Set.fromList
            [ hz
            | qual <- scanQualifiers parsed
            , hz <- comparisonHazards qual
            ]


-- | The scan\/join qualifier subtrees: the clauses Postgres uses to
-- choose an access path. The generic walk finds each enclosing statement
-- wherever it is nested, so subqueries, CTEs and MERGE are all covered;
-- the few clause fields below are the only hand-written part, and a
-- missing one is a false negative, never a crash. HAVING is deliberately
-- excluded — it filters after grouping and never gates the scan.
scanQualifiers :: Pg.ParseResult -> [Pg.Node]
scanQualifiers parsed =
    concat
        [ universeOfType @Pg.SelectStmt parsed
            >>= \s -> maybeToList (s ^. Pg.maybe'whereClause)
        , universeOfType @Pg.UpdateStmt parsed
            >>= \s -> maybeToList (s ^. Pg.maybe'whereClause)
        , universeOfType @Pg.DeleteStmt parsed
            >>= \s -> maybeToList (s ^. Pg.maybe'whereClause)
        , universeOfType @Pg.JoinExpr parsed
            >>= \j -> maybeToList (j ^. Pg.maybe'quals)
        , universeOfType @Pg.MergeStmt parsed
            >>= \m -> maybeToList (m ^. Pg.maybe'joinCondition)
        , universeOfType @Pg.MergeWhenClause parsed
            >>= \w -> maybeToList (w ^. Pg.maybe'condition)
        , universeOfType @Pg.IndexStmt parsed
            >>= \i -> maybeToList (i ^. Pg.maybe'whereClause)
        , universeOfType @Pg.OnConflictClause parsed
            >>= \o -> maybeToList (o ^. Pg.maybe'whereClause)
        ]


-- | Every index hazard that is a direct operand of a comparison
-- somewhere inside the given qualifier subtree. The generic walk descends
-- boolean nesting (@AND@\/@OR@) and any subquery quals, so the comparison
-- can be arbitrarily deep.
comparisonHazards :: Pg.Node -> [(IndexHazard, Int32)]
comparisonHazards qual =
    [ hz
    | ae <- universeOfType @Pg.A_Expr qual
    , isComparisonKind (ae ^. Pg.kind)
    , operand <- maybeToList (ae ^. Pg.maybe'lexpr) <> maybeToList (ae ^. Pg.maybe'rexpr)
    , Just hz <- [operandHazard operand]
    ]


-- | Classify a comparison operand. A bare 'ColumnRef' (or a literal) is
-- fine — the hazard is an /expression/ wrapping a column: a cast of a
-- column, or a function call taking a column argument.
operandHazard :: Pg.Node -> Maybe (IndexHazard, Int32)
operandHazard operand
    | Just tc <- operand ^. Pg.maybe'typeCast
    , isColumnRefCast tc =
        Just (CastHazard, tc ^. Pg.location)
    | Just fc <- operand ^. Pg.maybe'funcCall
    , funcCallTakesColumn fc =
        Just (FuncHazard, fc ^. Pg.location)
    | otherwise = Nothing


-- | A 'Pg.TypeCast' is a hazard when its @arg@ is a bare 'ColumnRef' —
-- @col::type@ — the shape that defeats an index.
isColumnRefCast :: Pg.TypeCast -> Bool
isColumnRefCast tc = isJust (view Pg.maybe'columnRef (tc ^. Pg.arg))


-- | A 'Pg.FuncCall' is a hazard when one of its arguments references a
-- column — @lower(id)@, @date_trunc('day', created_at)@ — since the
-- function result, not the column, is what the comparison sees. Argument
-- references only (the function /name/ is not a column), and functions
-- over only constants (@now()@, @lower('x')@) are not hazards.
funcCallTakesColumn :: Pg.FuncCall -> Bool
funcCallTakesColumn fc =
    not (all (null . universeOfType @Pg.ColumnRef) (fc ^.. Pg.args . traverse))


-- | The 'Pg.A_Expr' kinds that express a comparison the planner could
-- otherwise satisfy with an index: operators (incl. @ANY@\/@ALL@),
-- @IS [NOT] DISTINCT@, @IN@, @LIKE@\/@ILIKE@ and @BETWEEN@. @NULLIF@ and
-- @SIMILAR TO@ are excluded — neither is an index-usable scan qual.
isComparisonKind :: Pg.A_Expr_Kind -> Bool
isComparisonKind k =
    k
        `elem` [ Pg.AEXPR_OP
               , Pg.AEXPR_OP_ANY
               , Pg.AEXPR_OP_ALL
               , Pg.AEXPR_DISTINCT
               , Pg.AEXPR_NOT_DISTINCT
               , Pg.AEXPR_IN
               , Pg.AEXPR_LIKE
               , Pg.AEXPR_ILIKE
               , Pg.AEXPR_BETWEEN
               , Pg.AEXPR_NOT_BETWEEN
               , Pg.AEXPR_BETWEEN_SYM
               , Pg.AEXPR_NOT_BETWEEN_SYM
               ]


embeddedRange :: FilePath -> Int -> SourceRange
embeddedRange path line =
    SourceRange{path, startLine = line, startCol = 1, endLine = line, endCol = 1}


hazardFinding :: IndexHazard -> SourceRange -> Finding
hazardFinding hz range =
    Finding
        { range
        , severity = SeverityWarn
        , rule = ruleFor hz
        , title = titleFor hz
        , detail = detailFor hz
        , suppressedBy = Nothing
        }


ruleFor :: IndexHazard -> Rule
ruleFor = \case
    CastHazard -> IndexCastOnColumn
    FuncHazard -> IndexFunctionOnColumn


titleFor :: IndexHazard -> Text
titleFor = \case
    CastHazard -> "cast on a column reference may defeat an index"
    FuncHazard -> "function call on a column may defeat an index"


detailFor :: IndexHazard -> Text
detailFor = \case
    CastHazard ->
        """
        A column is cast to another type inside a WHERE/JOIN predicate, which
        forces Postgres to treat the column as an expression and prevents
        normal index lookups on it. If the column has an index, drop the cast
        (cast the other side of the comparison instead) or add a matching
        expression index. Suppress with `-- lint:ignore reason="…"` if the
        column is unindexed.
        """
    FuncHazard ->
        """
        A column is wrapped in a function call inside a WHERE/JOIN predicate,
        so Postgres compares the function result rather than the column and a
        plain index on the column cannot be used. Rewrite so the bare column
        is compared (move the function to the other side), or add a matching
        expression index. Suppress with `-- lint:ignore reason="…"` if the
        column is unindexed.
        """
