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
  freeVars
)
where

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)

type Name = T.Text
type Index = Int

data Expr
  = EName Name
  | EIndex Index
  | ELam Expr
  | EApp Expr Expr
  deriving (Show, Eq)

makeBaseFunctor ''Expr

freeVars :: Expr -> Set T.Text
freeVars = cata alg
  where
    alg :: ExprF (Set T.Text) -> Set T.Text
    alg (ENameF name) = S.singleton name
    alg fVars = foldr S.union S.empty fVars