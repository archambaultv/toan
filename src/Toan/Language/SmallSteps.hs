{-# LANGUAGE RankNTypes, ScopedTypeVariables, TupleSections, DeriveFunctor #-}

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


-- ana that applies the substitution directly (Maybe)
--   - repeat the procedure for Kleen closure
smallStep1 :: Expr -> Maybe Expr
smallStep1 = undefined

-- This might not return
normalize :: Expr -> Expr
normalize = last . smallSteps1

-- All the possible steps we can take
smallSteps1 :: Expr -> [Expr]
smallSteps1 t = ana coAlg t
  where coAlg :: Expr -> ListF Expr Expr
        coAlg x = 
          case smallStep1 x of
            Nothing -> Nil
            (Just x') -> Cons x' x'

-- ana that tells where to apply the substitution (SmallStep datatype)
     -- Procedure to push down the substitution
     -- repeat the procedure for Kleen closure, pushing down the previous closure
data SmallStepF f r
  = No (Fix f)
  | Yes (f r)
  | Beta (Fix f, Fix f)
  deriving (Functor)

type SmallStep f = Fix (SmallStepF f)

smallStep2 :: SmallStep ExprF -> SmallStep ExprF
smallStep2 = undefined

-- smallStep2Normalize . smallStep2 = smallStep1
smallStep2Normalize :: SmallStep ExprF -> Expr
smallStep2Normalize = undefined

-- data SmallStep2 = No | Yes | Beta