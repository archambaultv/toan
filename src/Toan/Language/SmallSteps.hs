{-# LANGUAGE RankNTypes, ScopedTypeVariables, TupleSections #-}

-- |
-- Module      :  Toan.Language.SmallSteps
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Language.SmallSteps (
  shift
  -- smallStep,
  -- smallSteps,
  -- normalize
)
where

import Control.Comonad
import Data.Functor.Foldable
import Toan.Fix.Attribute
import Toan.Language.Expr
import Data.Fix (Fix(..))

-- Increase all free variables by n
shift :: forall w 
      .  (Comonad w)
      => Integer -> AFix w ExprF -> AFix w ExprF
shift n t = ana coAlg (0, t)
  where coAlg :: CoAlg (Attribute w ExprF) (Integer, AFix w ExprF)
        coAlg = copyAttributeW 
              $ layerToAFix
              $ joinAccLayer countLambdas (algShift n)

algShift :: forall r . Integer -> Integer -> ExprF r -> ExprF r
algShift n nbOfLambdas (EIndexF x) =
  if x < nbOfLambdas
  then EIndexF x
  else EIndexF (x + n)
algShift _ _ x = x

-- Substitution for variable #0
subst :: forall w 
      .  (Comonad w)
      => AFix w ExprF -> AFix w ExprF -> AFix w ExprF
subst arg body = apo coAlg (0, body)
  where coAlg :: (Integer, AFix w ExprF) 
              -> (Attribute w ExprF (Either (AFix w ExprF) (Integer, AFix w ExprF)))
        coAlg = copyAttributeWM coAlg1

        coAlg1 :: (Integer, AFix w ExprF) 
              -> (ExprF (Either (AFix w ExprF) (Integer, AFix w ExprF)))
        coAlg1 = layerToAFixM layer

        layer :: forall r . (Integer, ExprF r)
              -> ExprF (Either (AFix w ExprF) (Integer, r))
        layer = joinAccLayerM countLambdas (algSubst (extractOne . flip shift arg))

algSubst :: (Integer -> ExprF a) -> Integer -> ExprF r -> ExprF (Either a r)
algSubst arg n (EIndexF x) = 
  case compare x n of
    EQ -> fmap Left $ arg n
    LT -> EIndexF x 
    GT -> EIndexF (x - 1)
algSubst _ _ x = fmap Right x

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