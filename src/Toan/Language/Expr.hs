{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell, TypeFamilies #-}

-- |
-- Module      :  Toan.Language.Expr
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Language.Expr (
  Name,
  Index,
  Expr(..),
  ExprF(..),
  algFreeVars
)
where

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Toan.Language.NExpr (Name)

type Index = Int

data Expr
  = EName Name
  | EIndex Index
  | ELam Expr
  | EApp Expr Expr
  deriving (Show, Eq)

makeBaseFunctor ''Expr

-- The functions below are to be used with cata directly with Expr or by
-- filtering with aFunctorF for AExpr.
-- ex:
--   freeVars :: Expr -> Set T.Text
--   freeVars = cata algFV
--   freeVarsA :: AExpr a -> Set T.Text
--   freeVarsA = cata (algFV . aFunctorF)

-- Compute the set of free variables
algFreeVars :: ExprF (Set T.Text) -> Set T.Text
algFreeVars (ENameF name) = S.singleton name
algFreeVars fVars = foldr S.union S.empty fVars