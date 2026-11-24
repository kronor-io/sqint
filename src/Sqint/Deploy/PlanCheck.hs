{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Plan-level cast check: prove a cast actually defeats an index by
-- comparing two @EXPLAIN@ plans of the same statement — one with the
-- cast, one with the cast removed.
--
-- For each embedded @PLpgSQL_stmt_execsql@ that "Sqint.Plpgsql"
-- surfaces, we:
--
-- 1. Walk the libpg_query parse tree for every @TypeCast@ whose arg
--    is a @ColumnRef@.
-- 2. If none, skip — this rule only has something to say about
--    column-side casts.
-- 3. Substitute every @NEW.x@ / @OLD.x@ 'ColumnRef' with a @$N@
--    'ParamRef' so the embedded DML can be planned outside its
--    trigger context.
-- 4. Build two variants of the statement: the original, and one
--    where every @TypeCast@ over a @ColumnRef@ has been replaced
--    with the bare 'ColumnRef'. Both go through
--    @pg_query_deparse_protobuf@ so the SQL is faithful to whatever
--    Postgres's regular SQL grammar accepts.
-- 5. Run @EXPLAIN (FORMAT JSON, COSTS OFF, GENERIC_PLAN)@ on both
--    variants. @GENERIC_PLAN@ is what lets us plan a query whose
--    parameters are unbound — without it Postgres compiles against
--    @NULL@ and short-circuits to @One-Time Filter: "false"@,
--    producing a plan that's useless for our diff.
-- 6. Walk both plan trees. A finding is emitted iff, for the same
--    table, the cast variant picks @Seq Scan@ / @Parallel Seq Scan@
--    and the no-cast variant picks any flavour of index scan.
--
-- The static @index/cast-on-column@ WARN still fires for every cast — the
-- two rules layer: WARN says "this looks suspicious", BLOCK says
-- "Postgres confirms the index isn't used".
module Sqint.Deploy.PlanCheck (
    checkFile,

    -- * Internals exposed for the test-suite
    Plan (..),
    Candidate (..),
    diffPlans,
    stripPartitionSuffix,
) where

import Data.Aeson qualified as Aeson
import Data.Char (isDigit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Any (..))
import Data.ProtoLens (defMessage)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Interpolate (iii)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as Text.IO
import PgQuery qualified as Pg
import Pqi qualified as PQ
import RIO
import RIO.State (State, evalState, state)
import Sqint.Deploy.Connection (DeploySettings, withConnection, withPqConnection)
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.Plpgsql (EmbeddedSql (..), extractEmbeddedSql)
import Sqint.Rule (Rule (..))


-- | Inspect every cast-bearing DML in @path@. Returns one BLOCK
-- finding per candidate whose differential EXPLAIN confirms an
-- index would be usable absent the column-side cast, plus one INFO
-- finding per candidate that couldn't be checked (deparse failure,
-- libpq error, etc.) so the failure surface is visible.
checkFile :: DeploySettings -> FilePath -> IO [Finding]
checkFile settings path = do
    src <- Text.IO.readFile path
    candidates <- collectCandidates path src
    if null candidates
        then pure []
        else withConnection settings \conn ->
            withPqConnection conn \pq ->
                concat <$> traverse (checkCandidate pq) candidates


-- | One DML the plan checker considers, located in the original
-- source so findings carry an accurate 'SourceRange'.
data Candidate = Candidate
    { sql :: Text
    , range :: SourceRange
    }


collectCandidates :: FilePath -> Text -> IO [Candidate]
collectCandidates path src = do
    embeds <- extractEmbeddedSql src
    pure
        [ Candidate
            { sql = e.sql
            , range = rangeAtLine path e.fileLine
            }
        | e <- embeds
        ]


rangeAtLine :: FilePath -> Int -> SourceRange
rangeAtLine path line =
    SourceRange
        { path
        , startLine = line
        , startCol = 1
        , endLine = line
        , endCol = 1
        }


-- ---------------------------------------------------------------------
-- Per-candidate check
-- ---------------------------------------------------------------------

checkCandidate :: PQ.Connection -> Candidate -> IO [Finding]
checkCandidate pq cand = do
    parsed <- Pg.parseSql (T.unpack cand.sql)
    case parsed of
        Left msg -> pure [skippedFinding cand ("libpg_query parse failed: " <> T.pack msg)]
        Right tree
            | not (hasColumnCast tree) -> pure []
            | otherwise -> runDifferential pq cand tree


hasColumnCast :: Pg.ParseResult -> Bool
hasColumnCast tree =
    any (nodeHasColumnCast . view Pg.stmt) (tree ^.. Pg.stmts . traverse)


nodeHasColumnCast :: Pg.Node -> Bool
nodeHasColumnCast node
    | Just tc <- view Pg.maybe'typeCast node
    , isColumnRef (tc ^. Pg.arg) =
        True
    | otherwise =
        getAny
            . getConst
            $ descendNode (Const . Any . nodeHasColumnCast) node


isColumnRef :: Pg.Node -> Bool
isColumnRef n = isJust (view Pg.maybe'columnRef n)


runDifferential :: PQ.Connection -> Candidate -> Pg.ParseResult -> IO [Finding]
runDifferential pq cand tree = do
    let treeWithParams = substituteNewOld tree
        treeNoCast = stripColumnCastsTree treeWithParams
    eOrig <- explainSql pq treeWithParams
    eNoCast <- explainSql pq treeNoCast
    case (eOrig, eNoCast) of
        (Right pOrig, Right pNoCast) ->
            pure (diffPlans cand pOrig pNoCast)
        (Left msg, _) -> pure [skippedFinding cand msg]
        (_, Left msg) -> pure [skippedFinding cand msg]


skippedFinding :: Candidate -> Text -> Finding
skippedFinding cand msg =
    Finding
        { range = cand.range
        , severity = SeverityInfo
        , rule = IndexPlanCheckSkipped
        , title = "Plan-level cast check skipped for this statement"
        , detail =
            [iii|
                The dynamic plan diff could not run on this statement; the
                static index/cast-on-column warning still applies. Reason:
                #{msg}
            |]
        , suppressedBy = Nothing
        }


-- ---------------------------------------------------------------------
-- Single recursive descent — used for both pure and stateful walks
-- ---------------------------------------------------------------------

-- | Apply an effectful transformation to every immediate 'Pg.Node'
-- child of the input 'Pg.Node', preserving the oneof discriminator.
-- Callers recurse by passing themselves as @f@; this function only
-- handles one level of descent.
--
-- 'Applicative' is enough — 'mapNode' below upgrades to 'Monad' for
-- bottom-up traversals that depend on the children's rewritten form.
descendNode :: Applicative f => (Pg.Node -> f Pg.Node) -> Pg.Node -> f Pg.Node
descendNode f node
    | Just v <- view Pg.maybe'selectStmt node = setSelect <$> descendSelect f v
    | Just v <- view Pg.maybe'updateStmt node = setUpdate <$> descendUpdate f v
    | Just v <- view Pg.maybe'deleteStmt node = setDelete <$> descendDelete f v
    | Just v <- view Pg.maybe'insertStmt node = setInsert <$> descendInsert f v
    | Just v <- view Pg.maybe'aExpr node = setAExpr <$> descendAExpr f v
    | Just v <- view Pg.maybe'boolExpr node = setBoolExpr <$> descendBoolExpr f v
    | Just v <- view Pg.maybe'funcCall node = setFuncCall <$> descendFuncCall f v
    | Just v <- view Pg.maybe'typeCast node = setTypeCast <$> descendTypeCast f v
    | Just v <- view Pg.maybe'caseExpr node = setCaseExpr <$> descendCaseExpr f v
    | Just v <- view Pg.maybe'caseWhen node = setCaseWhen <$> descendCaseWhen f v
    | Just v <- view Pg.maybe'coalesceExpr node = setCoalesce <$> descendCoalesce f v
    | Just v <- view Pg.maybe'nullTest node = setNullTest <$> descendNullTest f v
    | Just v <- view Pg.maybe'subLink node = setSubLink <$> descendSubLink f v
    | Just v <- view Pg.maybe'list node = setList <$> descendList f v
    | Just v <- view Pg.maybe'resTarget node = setResTarget <$> descendResTarget f v
    | otherwise = pure node
  where
    setSelect v = node & Pg.maybe'selectStmt .~ Just v
    setUpdate v = node & Pg.maybe'updateStmt .~ Just v
    setDelete v = node & Pg.maybe'deleteStmt .~ Just v
    setInsert v = node & Pg.maybe'insertStmt .~ Just v
    setAExpr v = node & Pg.maybe'aExpr .~ Just v
    setBoolExpr v = node & Pg.maybe'boolExpr .~ Just v
    setFuncCall v = node & Pg.maybe'funcCall .~ Just v
    setTypeCast v = node & Pg.maybe'typeCast .~ Just v
    setCaseExpr v = node & Pg.maybe'caseExpr .~ Just v
    setCaseWhen v = node & Pg.maybe'caseWhen .~ Just v
    setCoalesce v = node & Pg.maybe'coalesceExpr .~ Just v
    setNullTest v = node & Pg.maybe'nullTest .~ Just v
    setSubLink v = node & Pg.maybe'subLink .~ Just v
    setList v = node & Pg.maybe'list .~ Just v
    setResTarget v = node & Pg.maybe'resTarget .~ Just v


descendSelect :: Applicative f => (Pg.Node -> f Pg.Node) -> Pg.SelectStmt -> f Pg.SelectStmt
descendSelect f s =
    update
        <$> f (s ^. Pg.whereClause)
        <*> f (s ^. Pg.havingClause)
        <*> traverse f (s ^.. Pg.targetList . traverse)
        <*> traverse f (s ^.. Pg.fromClause . traverse)
  where
    update w h t fr =
        s
            & Pg.whereClause
            .~ w
                & Pg.havingClause
            .~ h
                & Pg.targetList
            .~ t
                & Pg.fromClause
            .~ fr


descendUpdate :: Applicative f => (Pg.Node -> f Pg.Node) -> Pg.UpdateStmt -> f Pg.UpdateStmt
descendUpdate f u =
    update
        <$> f (u ^. Pg.whereClause)
        <*> traverse f (u ^.. Pg.targetList . traverse)
        <*> traverse f (u ^.. Pg.fromClause . traverse)
        <*> traverse f (u ^.. Pg.returningList . traverse)
  where
    update w t fr r =
        u
            & Pg.whereClause
            .~ w
                & Pg.targetList
            .~ t
                & Pg.fromClause
            .~ fr
                & Pg.returningList
            .~ r


descendDelete :: Applicative f => (Pg.Node -> f Pg.Node) -> Pg.DeleteStmt -> f Pg.DeleteStmt
descendDelete f d =
    update
        <$> f (d ^. Pg.whereClause)
        <*> traverse f (d ^.. Pg.usingClause . traverse)
        <*> traverse f (d ^.. Pg.returningList . traverse)
  where
    update w using r =
        d
            & Pg.whereClause
            .~ w
                & Pg.usingClause
            .~ using
                & Pg.returningList
            .~ r


descendInsert :: Applicative f => (Pg.Node -> f Pg.Node) -> Pg.InsertStmt -> f Pg.InsertStmt
descendInsert f i =
    update
        <$> f (i ^. Pg.selectStmt)
        <*> traverse f (i ^.. Pg.returningList . traverse)
  where
    update sel r = i & Pg.selectStmt .~ sel & Pg.returningList .~ r


descendAExpr :: Applicative f => (Pg.Node -> f Pg.Node) -> Pg.A_Expr -> f Pg.A_Expr
descendAExpr f e =
    (\l r -> e & Pg.lexpr .~ l & Pg.rexpr .~ r)
        <$> f (e ^. Pg.lexpr)
        <*> f (e ^. Pg.rexpr)


descendBoolExpr :: Applicative f => (Pg.Node -> f Pg.Node) -> Pg.BoolExpr -> f Pg.BoolExpr
descendBoolExpr f e =
    (\xs -> e & Pg.args .~ xs) <$> traverse f (e ^.. Pg.args . traverse)


descendFuncCall :: Applicative f => (Pg.Node -> f Pg.Node) -> Pg.FuncCall -> f Pg.FuncCall
descendFuncCall f e =
    (\xs -> e & Pg.args .~ xs) <$> traverse f (e ^.. Pg.args . traverse)


descendTypeCast :: Applicative f => (Pg.Node -> f Pg.Node) -> Pg.TypeCast -> f Pg.TypeCast
descendTypeCast f tc =
    (\a -> tc & Pg.arg .~ a) <$> f (tc ^. Pg.arg)


descendCaseExpr :: Applicative f => (Pg.Node -> f Pg.Node) -> Pg.CaseExpr -> f Pg.CaseExpr
descendCaseExpr f e =
    (\a xs d -> e & Pg.arg .~ a & Pg.args .~ xs & Pg.defresult .~ d)
        <$> f (e ^. Pg.arg)
        <*> traverse f (e ^.. Pg.args . traverse)
        <*> f (e ^. Pg.defresult)


descendCaseWhen :: Applicative f => (Pg.Node -> f Pg.Node) -> Pg.CaseWhen -> f Pg.CaseWhen
descendCaseWhen f cw =
    (\e r -> cw & Pg.expr .~ e & Pg.result .~ r)
        <$> f (cw ^. Pg.expr)
        <*> f (cw ^. Pg.result)


descendCoalesce :: Applicative f => (Pg.Node -> f Pg.Node) -> Pg.CoalesceExpr -> f Pg.CoalesceExpr
descendCoalesce f e =
    (\xs -> e & Pg.args .~ xs) <$> traverse f (e ^.. Pg.args . traverse)


descendNullTest :: Applicative f => (Pg.Node -> f Pg.Node) -> Pg.NullTest -> f Pg.NullTest
descendNullTest f e =
    (\a -> e & Pg.arg .~ a) <$> f (e ^. Pg.arg)


descendSubLink :: Applicative f => (Pg.Node -> f Pg.Node) -> Pg.SubLink -> f Pg.SubLink
descendSubLink f e =
    (\t s -> e & Pg.testexpr .~ t & Pg.subselect .~ s)
        <$> f (e ^. Pg.testexpr)
        <*> f (e ^. Pg.subselect)


descendList :: Applicative f => (Pg.Node -> f Pg.Node) -> Pg.List -> f Pg.List
descendList f l =
    (\xs -> l & Pg.items .~ xs) <$> traverse f (l ^.. Pg.items . traverse)


descendResTarget :: Applicative f => (Pg.Node -> f Pg.Node) -> Pg.ResTarget -> f Pg.ResTarget
descendResTarget f rt =
    (\v -> rt & Pg.val .~ v) <$> f (rt ^. Pg.val)


-- | Bottom-up node traversal. Children are rewritten first, then
-- the function is applied to the (already-rewritten) node.
mapNode :: Monad m => (Pg.Node -> m Pg.Node) -> Pg.Node -> m Pg.Node
mapNode f node = descendNode (mapNode f) node >>= f


traverseStmts :: Monad m => (Pg.Node -> m Pg.Node) -> Pg.ParseResult -> m Pg.ParseResult
traverseStmts f tree = do
    rewritten <- traverse rewriteRaw (tree ^.. Pg.stmts . traverse)
    pure (tree & Pg.stmts .~ rewritten)
  where
    rewriteRaw r = do
        n <- mapNode f (r ^. Pg.stmt)
        pure (r & Pg.stmt .~ n)


-- ---------------------------------------------------------------------
-- AST mutations
-- ---------------------------------------------------------------------

-- | Replace every @NEW.<col>@ / @OLD.<col>@ 'ColumnRef' with a
-- numbered @$N@ 'ParamRef'. Numbering starts at 1 and follows
-- source order; the leftover counter is discarded — we don't need
-- the (param, column) ledger because @EXPLAIN (GENERIC_PLAN)@ types
-- parameters from how they're used in the query.
substituteNewOld :: Pg.ParseResult -> Pg.ParseResult
substituteNewOld tree = evalState (traverseStmts substituteOne tree) 1


substituteOne :: Pg.Node -> State Int32 Pg.Node
substituteOne node =
    case view Pg.maybe'columnRef node of
        Just cr | newOrOld cr -> paramRefNode <$> nextParam
        _ -> pure node


nextParam :: State Int32 Int32
nextParam = state \i -> (i, i + 1)


-- | A 'Pg.ColumnRef' refers to @NEW@/@OLD@ when it has exactly two
-- @String@ field parts and the first is @"new"@ or @"old"@
-- (case-insensitive).
newOrOld :: Pg.ColumnRef -> Bool
newOrOld cr =
    case cr ^.. Pg.fields . traverse of
        [a, _] | Just alias <- stringVal a -> T.toLower alias `elem` ["new", "old"]
        _ -> False


stringVal :: Pg.Node -> Maybe Text
stringVal n = view Pg.sval <$> view Pg.maybe'string n


paramRefNode :: Int32 -> Pg.Node
paramRefNode n =
    defMessage & Pg.paramRef .~ (defMessage & Pg.number .~ n)


-- | Apply 'stripColumnCasts' to every statement in the parse tree.
-- This is the pure, top-down walker — the only mutation is replacing
-- a @TypeCast(ColumnRef)@ with its bare 'ColumnRef'.
stripColumnCastsTree :: Pg.ParseResult -> Pg.ParseResult
stripColumnCastsTree = runIdentity . traverseStmts (Identity . stripColumnCasts)


stripColumnCasts :: Pg.Node -> Pg.Node
stripColumnCasts node
    | Just tc <- view Pg.maybe'typeCast node
    , isColumnRef (tc ^. Pg.arg) =
        tc ^. Pg.arg
    | otherwise =
        runIdentity (descendNode (Identity . stripColumnCasts) node)


-- ---------------------------------------------------------------------
-- EXPLAIN
-- ---------------------------------------------------------------------

-- | Deparse the tree and run @EXPLAIN (FORMAT JSON, COSTS OFF,
-- GENERIC_PLAN)@ directly on it. @GENERIC_PLAN@ tells Postgres to
-- build a plan whose parameters are unbound — exactly the case we
-- want, because a plan compiled against a specific @NULL@ value
-- short-circuits to @One-Time Filter: false@ instead of choosing
-- between Seq Scan / Index Scan.
explainSql :: PQ.Connection -> Pg.ParseResult -> IO (Either Text Plan)
explainSql pq tree = do
    deparsed <- Pg.deparseProtobuf tree
    case deparsed of
        Left e -> pure (Left ("deparse: " <> T.pack e))
        Right sql -> do
            let stmt =
                    "EXPLAIN (FORMAT JSON, COSTS OFF, GENERIC_PLAN) "
                        <> stripTrailingSemicolon sql
            result <- exec pq (TE.encodeUtf8 stmt)
            case result of
                Left err -> pure (Left err)
                Right [planBytes] ->
                    case Aeson.eitherDecodeStrict planBytes of
                        Left e -> pure (Left ("decode plan: " <> T.pack e))
                        Right [ExplainEnvelope p] -> pure (Right p)
                        Right _ -> pure (Left "EXPLAIN: unexpected plan-array shape")
                Right _ -> pure (Left "EXPLAIN: expected single plan row")


-- | Run a simple-query statement; on @TuplesOk@ return the first
-- column of every row.
exec :: PQ.Connection -> ByteString -> IO (Either Text [ByteString])
exec pq sql = do
    mResult <- PQ.exec pq sql
    case mResult of
        Nothing -> do
            mErr <- PQ.errorMessage pq
            pure (Left (maybe "libpq: no result" TE.decodeUtf8Lenient mErr))
        Just r -> do
            status <- PQ.resultStatus r
            case status of
                PQ.CommandOk -> pure (Right [])
                PQ.TuplesOk -> do
                    nRows <- PQ.ntuples r
                    rows <- forM [0 .. nRows - 1] \i ->
                        fromMaybe mempty <$> PQ.getvalue r i 0
                    pure (Right rows)
                _ -> do
                    mErr <- PQ.resultErrorMessage r
                    pure (Left (maybe (tshow status) TE.decodeUtf8Lenient mErr))


stripTrailingSemicolon :: Text -> Text
stripTrailingSemicolon = T.stripEnd . T.dropWhileEnd (== ';') . T.stripEnd


-- ---------------------------------------------------------------------
-- Typed plan tree
-- ---------------------------------------------------------------------

-- | @EXPLAIN (FORMAT JSON)@ returns @[{"Plan": {…}}]@. We decode the
-- envelope and then operate on the inner 'Plan'.
newtype ExplainEnvelope = ExplainEnvelope Plan


instance Aeson.FromJSON ExplainEnvelope where
    parseJSON = Aeson.withObject "ExplainEnvelope" \o ->
        ExplainEnvelope <$> o Aeson..: "Plan"


-- | The subset of a Postgres plan node we use. Other fields (@Costs@,
-- @Filter@, @Async Capable@, …) are intentionally not in the type —
-- if we ever care about them, adding a field here is the right
-- pressure point for the change. The recursive @Plans@ field carries
-- nested subplans (e.g. partition children under an @Append@).
data Plan = Plan
    { nodeType :: Text
    , relationName :: Maybe Text
    , indexName :: Maybe Text
    , plans :: [Plan]
    }


instance Aeson.FromJSON Plan where
    parseJSON = Aeson.withObject "Plan" \o ->
        Plan
            <$> o
            Aeson..: "Node Type"
            <*> o
            Aeson..:? "Relation Name"
            <*> o
            Aeson..:? "Index Name"
            <*> o
            Aeson..:? "Plans"
            Aeson..!= []


-- ---------------------------------------------------------------------
-- Plan diff
-- ---------------------------------------------------------------------

-- | A scan node we care about: Seq Scan or any flavour of Index Scan.
data ScanNode = ScanNode
    { nodeType :: Text
    , relation :: Text
    , indexName :: Maybe Text
    }


-- | Flatten a plan tree to every scan node that names a relation.
collectScans :: Plan -> [ScanNode]
collectScans p =
    here <> concatMap collectScans p.plans
  where
    here = case p.relationName of
        Just rel ->
            [ ScanNode
                { nodeType = p.nodeType
                , relation = rel
                , indexName = p.indexName
                }
            ]
        Nothing -> []


-- | At most one finding per candidate: when /any/ relation scanned
-- sequentially in the cast plan would be index-scanned in the
-- no-cast plan, emit one BLOCK and name the affected root tables.
--
-- Multi-relation correctness: a relation can appear multiple times in
-- one plan (self-join, @UPDATE … FROM same_table@, Append over
-- partitions). We compare /counts/ per relation rather than relying
-- on @Map.fromList@ to pick a representative — a relation is
-- "defeated" iff the cast plan has strictly more Seq Scans on it
-- than the no-cast plan AND the no-cast plan has at least one Index
-- Scan on the same relation.
diffPlans :: Candidate -> Plan -> Plan -> [Finding]
diffPlans cand orig noCast =
    let origScans = collectScans orig
        noCastScans = collectScans noCast

        countSeqByRelation :: [ScanNode] -> Map Text Int
        countSeqByRelation =
            Map.fromListWith (+) . map (\s -> (s.relation, 1)) . filter (isSeqScan . (.nodeType))

        relationsWithIndexScan :: [ScanNode] -> Set Text
        relationsWithIndexScan = Set.fromList . map (.relation) . filter (isIndexScan . (.nodeType))

        castSeq = countSeqByRelation origScans
        noCastSeq = countSeqByRelation noCastScans
        noCastIndexed = relationsWithIndexScan noCastScans

        defeated =
            [ relation
            | (relation, castN) <- Map.toList castSeq
            , castN > Map.findWithDefault 0 relation noCastSeq
            , Set.member relation noCastIndexed
            ]
     in case defeated of
            [] -> []
            _ -> [defeatedFinding cand defeated]


isSeqScan :: Text -> Bool
isSeqScan t = t `elem` ["Seq Scan", "Parallel Seq Scan"]


isIndexScan :: Text -> Bool
isIndexScan t =
    t
        `elem` [ "Index Scan"
               , "Index Only Scan"
               , "Bitmap Index Scan"
               , "Bitmap Heap Scan"
               , "Parallel Index Scan"
               , "Parallel Index Only Scan"
               ]


defeatedFinding :: Candidate -> [Text] -> Finding
defeatedFinding cand relations =
    let roots = nubOrd (map stripPartitionSuffix relations)
        rootList = T.intercalate ", " roots
     in Finding
            { range = cand.range
            , severity = SeverityBlock
            , rule = IndexDefeated
            , title = "Cast on a column forces a sequential scan on " <> rootList
            , detail =
                [iii|
                    Postgres EXPLAIN confirms a cast in this statement prevents
                    the planner from using an index on #{rootList}. With the
                    column cast removed the planner picks an Index Scan; with
                    the cast in place it falls back to Seq Scan. Cast the other
                    side of the comparison instead, or add an expression index.
                    Suppress with `-- lint:ignore reason="…"` if the cast is
                    intentional.
                |]
            , suppressedBy = Nothing
            }


-- | Reduce a per-partition table name to its root: strip a trailing
-- @_p<digits>@ /or/ @_default@ component. Used so a finding that hits
-- a partitioned table reads as "on @shop.event@" rather than
-- enumerating @event_p20260201@, @…p20260301@, …
stripPartitionSuffix :: Text -> Text
stripPartitionSuffix name
    | Just root <- T.stripSuffix "_default" name = root
    | otherwise = case T.breakOnEnd "_p" name of
        (before, digits)
            | not (T.null before)
            , not (T.null digits)
            , T.all isDigit digits ->
                T.dropEnd 2 before
        _ -> name
