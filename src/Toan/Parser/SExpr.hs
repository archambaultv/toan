{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

-- |
-- Module      :  Toan.Parser.Parser
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Parser.SExpr
( 
  Token(..),
  token,
  identifier,
  PSExpr,
  parseSExpr,
  decodeOne,
  decode
) where

import Data.Proxy (Proxy(..))
import Control.Applicative (empty)
import Data.Char (isAlphaNum, isSymbol)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Toan.SExpr.SExpr
import Toan.Fix.Annotate
import Toan.Parser.Location

-- A parsed SExpr is an expresssion of tokens with location annotation
data Token = TIdentifier T.Text
  deriving (Eq, Show)

token :: (M.MonadParsec e s m, M.Token s ~ Char) => m Token
token = fmap TIdentifier identifier

identifier :: forall e s m . (M.MonadParsec e s m, M.Token s ~ Char) => m T.Text
identifier = do
  c1 <- M.letterChar 
  cs <- M.takeWhileP Nothing (\c -> isAlphaNum c || isSymbol c)
  return $ T.pack $ (c1 : M.chunkToTokens (Proxy :: Proxy s) cs)

type PSExpr = AFix ((,) Location) (SExprF Token)

ws :: forall e s m .  (M.MonadParsec e s m, M.Token s ~ Char) => m ()
ws = L.space M.space1 
             (L.skipLineComment (M.tokenToChunk (Proxy :: Proxy s) ';'))
             empty

sexpStart :: (M.MonadParsec e s m, M.TraversableStream s, M.Token s ~ Char) => m (Located Char)
sexpStart = located $ M.char '('

sexpEnd :: (M.MonadParsec e s m, M.TraversableStream s, M.Token s ~ Char) => m (Located Char)
sexpEnd = located $ M.char ')'

-- | The 'parseSExprList' function return a parser for parsing S-expression of the form @'SList' _ _@.
sExprList :: (M.MonadParsec e s m, M.TraversableStream s, M.Token s ~ Char) => m PSExpr
sExprList = do
    ((Span pos1 _), _) <- L.lexeme ws sexpStart
    xs <- M.sepEndBy parseSExpr ws
    ((Span _ pos2), _) <- sexpEnd
    return $ AnnF ((Span pos1 pos2), (SListF xs))

-- | The 'parseSExpr' function return a parser for parsing
-- S-expression ('SExpr'), that is either an atom (@'SAtom' _@) or a
-- list @'SList' _ _@. See also 'decodeOne' and 'decode'.
sExprToken :: (M.MonadParsec e s m, M.TraversableStream s, M.Token s ~ Char) => m PSExpr
sExprToken = do
  (l, t) <- located token
  return $ AnnF (l, (SAtomF t))

-- | The 'parseSExpr' function return a parser for parsing
-- S-expression ('SExpr'), that is either an atom (@'SAtom' _@) or a
-- list @'SList' _ _@. See also 'decodeOne' and 'decode'.
parseSExpr :: (M.MonadParsec e s m, M.TraversableStream s, M.Token s ~ Char) => m PSExpr
parseSExpr = sExprToken M.<|> sExprList

-- | The 'decodeOne' function return a parser for parsing a file
-- containing only one S-expression ('SExpr'). It can parse extra
-- whitespace at the beginning and at the end of the file. See also
-- 'parseSExpr' and 'decode'.
decodeOne :: (M.MonadParsec e s m, M.TraversableStream s, M.Token s ~ Char) => m PSExpr
decodeOne =
  M.optional ws *> parseSExpr <* (M.optional ws >> M.eof)

-- | The 'decode' function return a parser for parsing a file
-- containing many S-expression ('SExpr'). It can parse extra
-- whitespace at the beginning and at the end of the file. See also
-- 'parseSExpr' and 'decodeOne'.
decode :: (M.MonadParsec e s m, M.TraversableStream s, M.Token s ~ Char) => m [PSExpr]
decode =
  M.optional ws *> M.sepEndBy parseSExpr ws <* M.eof