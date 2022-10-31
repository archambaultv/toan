{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell, TypeFamilies #-}

-- |
-- Module      :  Toan.SExpr.SExpr
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.SExpr.SExpr (
  SExpr(..),
  SExprF(..)
)
where

import Data.Bifunctor
import Data.Functor.Foldable.TH (makeBaseFunctor)

data SExpr a = SList [SExpr a]
             | SAtom a
  deriving (Eq, Show, Functor, Traversable, Foldable)

makeBaseFunctor ''SExpr

instance (Show a, Show r) => Show (SExprF a r) where
  show (SListF xs) = "SListF (" ++ show xs ++ ")"
  show (SAtomF a) = show a

instance Bifunctor SExprF where
  first foo (SAtomF a) = SAtomF (foo a)
  first _ (SListF xs) = (SListF xs)

  second foo x = fmap foo x