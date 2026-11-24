{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Generic, complete traversal of the @pg-query@ (proto-lens) parse
-- tree, built entirely from proto-lens field reflection.
--
-- The libpg_query AST is a single recursive @Node@ \"oneof\" with a few
-- hundred variants. A hand-written descent (@case@ on each
-- @maybe'selectStmt@ / @maybe'aExpr@ / … and listing every
-- expression-bearing field) is both boilerplate and lossy: any node
-- position the author forgot to enumerate becomes a silent false
-- negative — which, for a linter, means a dangerous migration passes
-- review.
--
-- proto-lens types have no 'Data'/'Generic' instance, so SYB/uniplate
-- can't traverse them. They do, however, expose enough /reflection/ to
-- build the traversal generically:
--
--   * 'fieldsByTag' enumerates every field of any message;
--   * pattern-matching 'MessageField' recovers a 'Message' dictionary
--     for message-typed fields, which is what lets us recurse without
--     knowing the concrete type;
--   * the 'FieldAccessor' lens pulls the value(s) out.
--
-- To hand a rule a /typed/ node we recover the concrete type by
-- round-tripping the existential through the wire format, guarded on
-- the message name. That needs only 'Message' (no 'Typeable'), and the
-- round-trip only runs for name matches, so the cost is one
-- serialise/parse per node we actually care about.
module Sqint.NodeWalk (
    universeOfType,
) where

import Data.ProtoLens.Encoding (decodeMessage, encodeMessage)
import Data.ProtoLens.Message (
    FieldAccessor (..),
    FieldDescriptor (..),
    FieldTypeDescriptor (..),
    Message,
    fieldsByTag,
    messageName,
 )
import RIO
import RIO.Map qualified as Map


-- | A proto message whose concrete type is hidden but whose 'Message'
-- dictionary is retained, so it can be recursed into or re-serialised.
data SomeMessage where
    SomeMessage :: Message m => m -> SomeMessage


-- | Every message of type @t@ reachable from @root@ (inclusive),
-- returned as the concrete, typed value. The proto-lens counterpart of
-- uniplate's @[x | x <- universe root]@ — but it works on types with no
-- 'Data' instance, and is complete by construction (it descends every
-- message-typed field of every node, so no AST variant can hide a
-- match).
universeOfType :: forall t root. (Message t, Message root) => root -> [t]
universeOfType root =
    [t | SomeMessage m <- universeMessages root, Just t <- [downcast m]]


-- | @root@ plus every transitive sub-message, discovered purely by
-- reflecting over each message's fields.
universeMessages :: Message root => root -> [SomeMessage]
universeMessages root =
    SomeMessage root
        : concatMap (\(SomeMessage child) -> universeMessages child) (childMessages root)


-- | The immediate sub-messages of any message — the generic
-- replacement for a hand-written @childNodes@.
childMessages :: forall m. Message m => m -> [SomeMessage]
childMessages msg = concatMap descend (Map.elems (fieldsByTag @m))
  where
    descend :: FieldDescriptor m -> [SomeMessage]
    descend (FieldDescriptor _ fieldType accessor) =
        case fieldType of
            -- Matching 'MessageField' brings @Message value@ into
            -- scope, which is what authorises the 'SomeMessage' below.
            MessageField _ -> SomeMessage <$> accessorValues accessor
            _ -> []

    accessorValues :: FieldAccessor m value -> [value]
    accessorValues = \case
        PlainField _ l -> [msg ^. l]
        OptionalField l -> maybeToList (msg ^. l)
        RepeatedField _ l -> msg ^. l
        -- The pg_query AST has no map-of-message fields; if one is ever
        -- added, the value would need recursing here.
        MapField{} -> []


-- | Recover a concrete message type from an existential by round-
-- tripping through the wire format, guarded on the message name so we
-- never decode an unrelated type. Needs only 'Message'.
downcast :: forall t s. (Message t, Message s) => s -> Maybe t
downcast s
    | messageName (Proxy @s) == messageName (Proxy @t) =
        either (const Nothing) Just (decodeMessage (encodeMessage s))
    | otherwise = Nothing
