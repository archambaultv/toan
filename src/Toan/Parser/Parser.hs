{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

-- |
-- Module      :  Toan.Parser.Parser
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Parser.Parser
( 
    parseSExpr,
    decodeOne,
    decode
) where

import Data.Proxy (Proxy(..))
import Control.Applicative (empty)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import Toan.SExpr.SExpr
import Toan.Parser.Token

ws :: forall e s m .  (M.MonadParsec e s m, M.Token s ~ Char) => m ()
ws = L.space M.space1 
             (L.skipLineComment (M.tokenToChunk (Proxy :: Proxy s) ';'))
             empty

sexpStart :: (M.MonadParsec e s m, M.Token s ~ Char) => m Char
sexpStart = M.char '('

sexpEnd :: (M.MonadParsec e s m, M.Token s ~ Char) => m Char
sexpEnd = M.char ')'

-- | The 'parseSExprList' function return a parser for parsing S-expression of the form @'SList' _ _@.
sExprList :: (M.MonadParsec e s m, M.Token s ~ Char) => m (SExpr Token)
sExprList = do
    _ <- L.lexeme ws sexpStart
    xs <- M.sepEndBy parseSExpr ws
    _ <- sexpEnd
    return $ SList xs

-- | The 'parseSExpr' function return a parser for parsing
-- S-expression ('SExpr'), that is either an atom (@'SAtom' _@) or a
-- list @'SList' _ _@. See also 'decodeOne' and 'decode'.
parseSExpr :: (M.MonadParsec e s m, M.Token s ~ Char) => m (SExpr Token)
parseSExpr = (SAtom <$> token) M.<|> sExprList

-- | The 'decodeOne' function return a parser for parsing a file
-- containing only one S-expression ('SExpr'). It can parse extra
-- whitespace at the beginning and at the end of the file. See also
-- 'parseSExpr' and 'decode'.
decodeOne :: (M.MonadParsec e s m, M.Token s ~ Char) => m (SExpr Token)
decodeOne =
  M.optional ws *> parseSExpr <* (M.optional ws >> M.eof)

-- | The 'decode' function return a parser for parsing a file
-- containing many S-expression ('SExpr'). It can parse extra
-- whitespace at the beginning and at the end of the file. See also
-- 'parseSExpr' and 'decodeOne'.
decode :: (M.MonadParsec e s m, M.Token s ~ Char) => m [SExpr Token]
decode =
  M.optional ws *> M.sepEndBy parseSExpr ws <* M.eof