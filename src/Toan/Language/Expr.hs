{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
    TemplateHaskell, TypeFamilies, ScopedTypeVariables #-}

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
  AExpr,
  nexprToExpr,
  -- aNexprToAExpr,
  algNExprToExpr,
  algFreeVars
)
where

import Data.Set (Set)
import Data.Functor.Foldable (cata, project)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Toan.Annotated
import Toan.Language.NExpr (Name, NExpr, NExprF(..), ANExpr)

type Index = Int

data Expr
  = EName Name
  | EIndex Index
  | ELam Expr
  | EApp Expr Expr
  deriving (Show, Eq)

makeBaseFunctor ''Expr

type AExpr a = Annotated a ExprF

nexprToExpr :: NExpr -> Expr
nexprToExpr e = cata algNExprToExpr e (HM.empty, 0)

data Fix f = Fix {unfix :: f (Fix f)}

newtype AnnotationF a b r = Annotation (a, b r)
  deriving (Eq, Show, Functor, Foldable, Traversable)
type Annotation a b = Fix (AnnotationF a b)

-- aNexprToAExpr :: forall a . ANExpr a -> AExpr a
-- aNexprToAExpr e = cata go e (HM.empty, 0)
--   where go :: AnnotatedF a NExprF ((HM.HashMap Name Index, Index) -> AExpr a)
--            -> (HM.HashMap Name Index, Index)
--            -> AExpr a
--         go (AnnotatedF (a, x)) m = 
--           let x' :: NExprF ((HM.HashMap Name Index, Index) -> Expr)
--               x' = fmap (noAnnotation .) x
--               e :: Expr
--               e = algNExprToExpr x' m
--           in Annotated (a, project e)

-- The functions below are to be used with cata directly with Expr or by
-- filtering with aFunctorF for Annotated x ExprF.
-- ex:
--   freeVars :: Expr -> Set T.Text
--   freeVars = cata algFV
--   freeVarsA :: AExpr a -> Set T.Text
--   freeVarsA = cata (algFV . aFunctorF)

-- Compute the set of free variables
algFreeVars :: ExprF (Set T.Text) -> Set T.Text
algFreeVars (ENameF name) = S.singleton name
algFreeVars fVars = foldr S.union S.empty fVars

-- Translate a named expression into an expression
algNExprToExpr :: NExprF ((HM.HashMap Name Index, Index) -> Expr) 
              -> (HM.HashMap Name Index, Index) 
              -> Expr
algNExprToExpr (NNameF x) (m, current) = 
  case HM.lookup x m of
    Nothing -> EName x
    (Just i) -> EIndex (current - i)
algNExprToExpr (NLamF x next) (m, current) = ELam $ next (HM.insert x current m, current + 1)
algNExprToExpr (NAppF next1 next2) acc = EApp (next1 acc) (next2 acc)