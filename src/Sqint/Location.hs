{-# LANGUAGE NoFieldSelectors #-}

-- | Turning libpg_query's byte offsets into the lines a finding reports.
--
-- libpg_query reports a statement as starting just past the previous
-- statement's semicolon (or at offset 0 for the first), so the span it
-- gives includes the whitespace and comments in between — the
-- @-- Deploy …@ header, a @-- lint:ignore@ above the statement. A finding
-- placed at that raw offset lands on the line of the previous statement,
-- and an annotation written above or beside the statement it means
-- misses. 'statementRange' skips that leading trivia and covers every line
-- of the statement, which is what suppressions match against.
--
-- Offsets are UTF-8 byte offsets, so all arithmetic here is on the
-- encoded bytes; columns are counted in characters.
module Sqint.Location (
    sourceRangeAt,
    statementRange,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Text.Encoding qualified as TE
import PgQuery qualified as Pg
import RIO
import Sqint.Diagnostic (SourceRange (..))


-- | A zero-width range at a byte offset — for a token's own location,
-- which libpg_query reports exactly.
sourceRangeAt :: Integral n => FilePath -> Text -> n -> SourceRange
sourceRangeAt path src offset =
    let (line, col) = position (TE.encodeUtf8 src) (clamp offset)
     in SourceRange{path, startLine = line, startCol = col, endLine = line, endCol = col}


-- | From a statement's first token to its last non-blank character.
statementRange :: FilePath -> Text -> Pg.RawStmt -> SourceRange
statementRange path src raw =
    let bytes = TE.encodeUtf8 src
        total = BS.length bytes
        loc = min total (clamp (raw ^. Pg.stmtLocation))
        len = clamp (raw ^. Pg.stmtLen)
        -- A length of 0 means "to the end of the input".
        end = if len == 0 then total else min total (loc + len)
        start = case skipTrivia bytes loc end of
            s | s < end -> s
            _ -> loc
        lastChar = max start (trimEnd bytes start end - 1)
        (startLine, startCol) = position bytes start
        (endLine, endCol) = position bytes lastChar
     in SourceRange{path, startLine, startCol, endLine, endCol}


-- | 1-based line and column of a byte offset.
position :: ByteString -> Int -> (Int, Int)
position bytes offset =
    let before = BS.take offset bytes
        line = 1 + BS8.count '\n' before
        lineStart = maybe 0 (+ 1) (BS8.elemIndexEnd '\n' before)
        col = 1 + BS.length (BS.filter isLeadByte (BS.drop lineStart before))
     in (line, col)
  where
    -- Count characters, not bytes: skip UTF-8 continuation bytes.
    isLeadByte b = b < 0x80 || b >= 0xC0


-- | The first offset at or after @i@ that is not whitespace or a comment.
skipTrivia :: ByteString -> Int -> Int -> Int
skipTrivia bytes i end
    | i >= end = end
    | isSpaceByte (BS.index bytes i) = skipTrivia bytes (i + 1) end
    | startsWith "--" = skipTrivia bytes (lineEnd (i + 2)) end
    | startsWith "/*" = skipTrivia bytes (blockEnd (1 :: Int) (i + 2)) end
    | otherwise = i
  where
    startsWith p = p `BS.isPrefixOf` BS.drop i bytes
    lineEnd j = maybe end (+ j) (BS8.elemIndex '\n' (BS.drop j (BS.take end bytes)))
    -- PostgreSQL block comments nest.
    blockEnd depth j
        | j >= end = end
        | depth == 0 = j
        | "*/" `BS.isPrefixOf` BS.drop j bytes = blockEnd (depth - 1) (j + 2)
        | "/*" `BS.isPrefixOf` BS.drop j bytes = blockEnd (depth + 1) (j + 2)
        | otherwise = blockEnd depth (j + 1)


-- | One past the last non-whitespace byte in @[start, end)@.
trimEnd :: ByteString -> Int -> Int -> Int
trimEnd bytes start end
    | end > start && isSpaceByte (BS.index bytes (end - 1)) = trimEnd bytes start (end - 1)
    | otherwise = end


isSpaceByte :: Word8 -> Bool
isSpaceByte b = b == 0x20 || (b >= 0x09 && b <= 0x0D)


clamp :: Integral n => n -> Int
clamp n = max 0 (fromIntegral n)
