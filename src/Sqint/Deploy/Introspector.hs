-- | Catalog-introspection types shared between the dynamic pass's executor
-- (raw libpq) and observer (Hasql) sides. The actual queries live next
-- to their consumers — see 'Sqint.Deploy.fetchRelFileNodes'
-- for the executor-side relfilenode snapshot and
-- 'Sqint.Deploy.Observer.listConnectionLocks' for the
-- observer-side @pg_locks@ query.
module Sqint.Deploy.Introspector (
    Pid (..),
    TableName (..),
    RelFileNode (..),
) where

import RIO


newtype Pid = Pid Int32


newtype TableName = TableName Text
    deriving stock (Eq, Ord)


newtype RelFileNode = RelFileNode Int64
    deriving stock (Eq)
