{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
    TemplateHaskell, PatternSynonyms #-}

-- |
-- Module      :  Toan.SExpr.SExpr
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.SExpr.SExpr (
  SExprF(..),
  SExpr,
  pattern SList,
  pattern SAtom
)
where

import Data.Bifunctor.TH
import Data.Fix (Fix(..))

data SExprF a r = SListF [r]
                | SAtomF a
  deriving (Eq, Show, Functor, Foldable, Traversable)

$(deriveBifunctor ''SExprF)

type SExpr a = Fix (SExprF a)

pattern SList :: [SExpr a] -> SExpr a
pattern SList x = Fix (SListF x)

pattern SAtom :: a -> SExpr a
pattern SAtom x = Fix (SAtomF x)