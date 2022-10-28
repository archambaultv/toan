{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

-- |
-- Module      :  Toan.Parser.Token
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Parser.Token (
  Token(..),
  token,
  identifier
)
where

import Data.Proxy (Proxy(..))
import Data.Char (isAlphaNum, isSymbol)
import qualified Data.Text as T
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Toan.Language.Expr

data Token = TIdentifier Name
  deriving (Eq, Show)

token :: (M.MonadParsec e s m, M.Token s ~ Char) => m Token
token = fmap TIdentifier identifier

identifier :: forall e s m . (M.MonadParsec e s m, M.Token s ~ Char) => m Name
identifier = do
  c1 <- M.letterChar 
  cs <- M.takeWhileP Nothing (\c -> isAlphaNum c || isSymbol c)
  return $ T.pack $ (c1 : M.chunkToTokens (Proxy :: Proxy s) cs)