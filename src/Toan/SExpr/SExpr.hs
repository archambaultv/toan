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

import Data.Functor.Foldable.TH (makeBaseFunctor)

data SExpr a = SList [SExpr a]
             | SAtom a
  deriving (Eq, Show, Functor, Traversable, Foldable)

makeBaseFunctor ''SExpr