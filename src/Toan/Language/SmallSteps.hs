-- |
-- Module      :  Toan.Language.SmallSteps
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Language.SmallSteps (
  -- subst,
  -- smallStep,
  -- smallSteps,
  -- normalize
)
where

import Control.Comonad
import Data.Functor.Foldable
import Toan.Fix.Fix
import Toan.Fix.Annotate
import Toan.Language.Expr

-- Increase all free variables by n
-- shift :: Int -> Expr -> Expr
-- shift n t = cata (functorToFix algShift) t (n, 0)

-- shiftA :: (Comonad w) => Int -> AFix w ExprF -> AFix w ExprF
-- shiftA n t = cata (functorToAFix algShift . runAnnotate) t (n, 0)

-- algShift :: ExprF ((Int, Int) -> r) -> ((Int, Int) -> ExprF r)
-- algShift (ENameF x) _ = ENameF x
-- algShift (EIndexF x) (n, i) =
--   if x < i
--   then EIndexF x
--   else EIndexF (x + n)
-- algShift (ELamF t1) (n, i) =
--   let t1' = t1 (n, i + 1)
--   in ELamF t1'
-- algShift (EAppF t1 t2) x =
--   let t1' = t1 x
--       t2' = t2 x
--   in EAppF t1' t2'

-- -- Substitution for variable #0
-- subst :: Expr -> Expr -> Expr
-- subst body arg = cata (functorToFix2 algSubst) body (flip shift arg, 0)

-- substA :: (Comonad w) => (AFix w ExprF) -> (AFix w ExprF) -> (AFix w ExprF)
-- substA body arg = cata (functorToAFix2 algSubst . runAnnotate) body (flip shiftA arg, 0)

-- algSubst :: ExprF ((Int -> a, Int) -> r) -> (Int -> a, Int) -> Either a (ExprF r)
-- algSubst (ENameF x) _ = Right $ ENameF x
-- algSubst (EIndexF x) (arg, n) = 
--   if x == n 
--   then Left $ arg n
--   else Right $ if x < n then EIndexF x else EIndexF (x - 1)
-- algSubst (ELamF t1) (arg, n) = 
--   let t1' = t1 (arg, n + 1)
--   in Right $ ELamF t1'
-- algSubst (EAppF t1 t2) x =
--   let t1' = t1 x
--       t2' = t2 x
--   in Right $ EAppF t1' t2'

-- -- Takes a small step if possible
-- smallStep :: Expr -> Maybe Expr
-- smallStep = para alg
--   where alg :: ExprF (Expr, Maybe Expr) -> Maybe Expr
--         alg (ENameF _) = Nothing
--         alg (EIndexF _) = Nothing
--         alg (ELamF (_, t1)) = ELam <$> t1
--         alg (EAppF (ELam body, _) (arg, _)) = Just $ subst body arg
--         alg (EAppF (t1, t1M) (t2, t2M)) = 
--           case t1M of
--             (Just t1') -> Just $ EApp t1' t2
--             Nothing -> (EApp t1) <$> t2M

-- -- This might not return
-- normalize :: Expr -> Expr
-- normalize = last . smallSteps

-- -- All the possible steps we can take
-- smallSteps :: Expr -> [Expr]
-- smallSteps t = t : ana coAlg t
--   where coAlg :: Expr -> ListF Expr Expr
--         coAlg x = 
--           case smallStep x of
--             Nothing -> Nil
--             (Just x') -> Cons x' x'