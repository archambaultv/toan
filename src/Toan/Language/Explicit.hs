{-# LANGUAGE RankNTypes, ScopedTypeVariables, TupleSections, DeriveFunctor,
    PatternSynonyms #-}

-- |
-- Module      :  Toan.Language.SmallSteps
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Language.Explicit (
  shift
  -- smallStep,
  -- smallSteps,
  -- normalize
)
where

import Control.Comonad
import Data.Functor.Foldable
import Data.Fix (Fix(..))
import Toan.Fix.Attribute
import Toan.Language.Expr
import Toan.Language.SmallSteps


betaStep :: SmallStep ExprF -> SmallStep ExprF
betaStep (No t) = para betaAlg t
betaStep (Yes f) = Yes (fmap betaStep f)
betaStep (Subst body arg) = applyBetaNTrans 
                          $ toFunctor
                          $ twoStep (Subst body arg)

twoStep :: SmallStep ExprF -> SmallStep ExprF
twoStep x = fmap oneStep (oneStep x)

-- FixMe : Should push the new accumulator
oneStep :: SmallStep ExprF -> SmallStep ExprF
oneStep (No t) = (No t)
oneStep (Yes f) = Yes (fmap oneStep f)
oneStep (Subst body arg) = 
  let fBody = unFix body
      nbOfLambdas = 0
      acc = countLambdas nbOfLambdas fBody
      f = algSubst (unFix . flip shift' arg) nbOfLambdas fBody
  in Yes (fmap (eitherToSmallStep body) f)

eitherToSmallStep :: Expr -> Either Expr Expr -> SmallStep ExprF
eitherToSmallStep _ (Left x) = No x
eitherToSmallStep arg (Right x) = Subst arg x