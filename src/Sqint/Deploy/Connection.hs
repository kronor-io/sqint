{-# LANGUAGE OverloadedStrings #-}

-- | Thin wrappers around 'Hasql.Connection' so the deploy modules can
-- acquire/release per-purpose connections (executor + observer) without
-- repeating boilerplate. Each dynamic-pass run opens one or two short
-- connections and closes them immediately afterwards; we don't need a
-- pool for what is essentially a CI batch tool.
module Sqint.Deploy.Connection (
    DeploySettings (..),
    parseSettings,
    withConnection,
    withPqConnection,
    runSession,
    ConnectionError (..),
) where

import Data.Text qualified as T
import Hasql.Connection (Connection)
import Hasql.Connection qualified as Conn
import Hasql.Connection.Settings qualified as Settings
import Hasql.Errors qualified
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Pqi qualified
import Pqi.Ffi qualified
import RIO


-- | Connection parameters for the dynamic pass. Always supplied as a libpq
-- DSN/URI; the static linter path stays free of this type so we never
-- accidentally require Postgres for pre-commit usage.
newtype DeploySettings = DeploySettings {dsn :: Text}


-- | Reject empty input and otherwise forward the string to libpq via
-- 'Settings.connectionString', which accepts both @postgres://@ URIs
-- and @key=value@ DSNs. Format validation is deferred to libpq — at
-- this layer any non-empty string is structurally valid; the actual
-- syntactic check happens when 'withConnection' calls @Conn.acquire@.
parseSettings :: String -> Either String DeploySettings
parseSettings input
    | null input = Left "empty connection string"
    | otherwise = Right (DeploySettings (T.pack input))


data ConnectionError
    = AcquireFailed Text
    | SessionFailed Hasql.Errors.SessionError
    deriving stock (Show)


instance Exception ConnectionError


-- | Acquire a connection, run the action, release the connection. Even
-- on exceptions, the connection is released.
withConnection :: DeploySettings -> (Connection -> IO a) -> IO a
withConnection DeploySettings{dsn} =
    bracket acquire Conn.release
  where
    settings = Settings.connectionString dsn

    -- libpq, through pqi's FFI binding: the dynamic pass needs TLS for
    -- a remote database, which pqi's native driver does not offer.
    acquire = do
        result <- Conn.acquire Pqi.Ffi.adapter settings
        case result of
            Right conn -> pure conn
            Left err -> throwIO (AcquireFailed (tshow err))


-- | Run a 'Session' against a connection, throwing on errors.
runSession :: Connection -> Session a -> IO a
runSession conn s = do
    result <- Conn.use conn s
    case result of
        Right value -> pure value
        Left err -> throwIO (SessionFailed err)


-- | Borrow the driver connection underlying a hasql connection, for the
-- statements the dynamic pass sends as-is (@EXPLAIN@, savepoints, a whole
-- script through the simple-query protocol) rather than as a prepared
-- 'Session'.
--
-- The hasql connection is locked for the whole callback, so nothing else
-- may use it meanwhile.
withPqConnection :: Connection -> (Pqi.Connection -> IO a) -> IO a
withPqConnection conn act = do
    result <-
        Conn.use
            conn
            (Session.onLibpqConnection \pq -> (,pq) . Right <$> act pq)
    either (throwIO . SessionFailed) pure result
