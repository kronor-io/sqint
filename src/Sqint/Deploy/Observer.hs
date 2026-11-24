{-# LANGUAGE MultilineStrings #-}

-- | Query @pg_locks@ from a side connection to learn what locks a target
-- backend (the executor) is currently holding. Used immediately after
-- the executor runs the candidate statement and before its transaction
-- ends — the locks acquired by the statement are still in
-- @pg_locks@.
module Sqint.Deploy.Observer (
    listConnectionLocks,
    PidLock (..),
) where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import Hasql.Statement qualified as Statement
import RIO
import Sqint.Deploy.Introspector (Pid (..), TableName (..))


data PidLock = PidLock
    { table :: TableName
    , mode :: Text
    }
    deriving stock (Eq, Ord)


-- | List the relation-level locks held by a specific backend
-- ('Pid'). Restricted to relations outside system schemas so we don't
-- surface noise from catalog tables that every transaction touches.
listConnectionLocks :: Pid -> Session [PidLock]
listConnectionLocks (Pid pid) = do
    rows <- Session.statement pid locksHeldBy
    pure
        $ [ PidLock{table = TableName t, mode = m}
          | (t, m) <- Vector.toList rows
          ]


locksHeldBy :: Statement Int32 (Vector (Text, Text))
locksHeldBy =
    Statement.preparable
        """
        select
            (quote_ident(n.nspname) || '.' || quote_ident(c.relname))::text
          , l.mode::text
        from pg_catalog.pg_locks l
        join pg_catalog.pg_class c
          on c.oid = l.relation
        join pg_catalog.pg_namespace n
          on n.oid = c.relnamespace
        where l.pid = $1::int4
          and n.nspname not in ('pg_catalog', 'information_schema', 'pg_toast')
          and n.nspname not like 'pg_temp_%'
          and n.nspname not like 'pg_toast_temp_%'
          and c.relkind in ('r', 'p')
          and l.locktype = 'relation'
          and l.granted
        """
        (Encoders.param (Encoders.nonNullable Encoders.int4))
        (Decoders.rowVector ((,) <$> text <*> text))
  where
    text = Decoders.column (Decoders.nonNullable Decoders.text)
