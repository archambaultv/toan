{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
    PatternSynonyms, ScopedTypeVariables #-}

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
  ExprF(..),
  Expr,
  pattern EName,
  pattern EIndex,
  pattern ELam,
  pattern EApp,
  nexprToExpr,
  -- aNexprToAExpr,
  algNExprToExpr,
  algFreeVars
)
where

import Data.Fix (Fix(..))
import Data.Set (Set)
import Data.Functor.Foldable (cata)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Toan.Language.NExpr (Name, NExpr, NExprF(..))

type Index = Int

data ExprF r
  = ENameF Name
  | EIndexF Index
  | ELamF r
  | EAppF r r
  deriving (Show, Eq, Functor, Foldable, Traversable)

type Expr = Fix ExprF

pattern EName :: Name -> Expr
pattern EName x = Fix (ENameF x)

pattern EIndex :: Index -> Expr
pattern EIndex x = Fix (EIndexF x)

pattern ELam :: Expr -> Expr
pattern ELam x = Fix (ELamF x)

pattern EApp :: Expr -> Expr -> Expr
pattern EApp e1 e2 = Fix (EAppF e1 e2)

nexprToExpr :: NExpr -> Expr
nexprToExpr e = cata algNExprToExpr e (HM.empty, 0)

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