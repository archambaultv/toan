-- |
-- Module      :  Toan.Language.SmallSteps
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Language.SmallSteps (
  subst,
  smallStep,
  smallSteps,
  normalize
)
where

import Data.Functor.Foldable
import Toan.Language.Expr

-- Increase all free variables by n
shift :: Int -> Expr -> Expr
shift n t = cata alg t 0
  where alg :: ExprF (Int -> Expr) -> (Int -> Expr)
        alg (ENameF x) _ = EName x
        alg (EIndexF x) i =
          if x < i
          then EIndex x
          else EIndex (x + n)
        alg (ELamF t1) i =
          let t1' = t1 (i + 1)
          in ELam t1'
        alg (EAppF t1 t2) i =
          let t1' = t1 i
              t2' = t2 i
          in EApp t1' t2' 

-- Substitution for variable #0
subst :: Expr -> Expr -> Expr
subst body arg = cata alg body 0
  where alg :: ExprF (Int -> Expr) -> (Int -> Expr)
        alg (ENameF x) _ = EName x
        alg (EIndexF x) n = 
          if x == n 
          then shift n arg 
          else if x < n then EIndex x else EIndex (x - 1)
        alg (ELamF t1) n = 
          let t1' = t1 (n + 1)
          in ELam t1'
        alg (EAppF t1 t2) n =
          let t1' = t1 n
              t2' = t2 n
          in EApp t1' t2'
          
-- Takes a small step if possible
smallStep :: Expr -> Maybe Expr
smallStep = para alg
  where alg :: ExprF (Expr, Maybe Expr) -> Maybe Expr
        alg (ENameF _) = Nothing
        alg (EIndexF _) = Nothing
        alg (ELamF (_, t1)) = ELam <$> t1
        alg (EAppF (ELam body, _) (arg, _)) = Just $ subst body arg
        alg (EAppF (t1, t1M) (t2, t2M)) = 
          case t1M of
            (Just t1') -> Just $ EApp t1' t2
            Nothing -> (EApp t1) <$> t2M

-- This might not return
normalize :: Expr -> Expr
normalize = last . smallSteps

-- All the possible steps we can take
smallSteps :: Expr -> [Expr]
smallSteps t = t : ana coAlg t
  where coAlg :: Expr -> ListF Expr Expr
        coAlg x = 
          case smallStep x of
            Nothing -> Nil
            (Just x') -> Cons x' x'