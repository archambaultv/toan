{-# LANGUAGE RankNTypes, ScopedTypeVariables, TupleSections, DeriveFunctor,
    PatternSynonyms #-}

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

-- Parallel substitution, the first one from the top for each branch
congruence :: Expr -> SmallStep ExprF
congruence = para alg
  where
    alg :: AlgW ExprF ((,) Expr) (SmallStep ExprF)
    alg (EAppF (ELam e,_) (x, _)) =  Subst e x
    alg x = 
      let hasSubst = foldr (\a c -> (noAsFalse $ snd a) || c ) False x
      in if hasSubst
         then Yes $ fmap snd x
         else No $ Fix $ fmap fst x

evaluate :: SmallStep ExprF -> Expr
evaluate = apo coAlg
  where
    coAlg :: CoAlgM ExprF (Either Expr) (SmallStep ExprF)
    coAlg (No t) = fmap Left $ unFix t
    coAlg (Yes f) = fmap Right f
    coAlg (Subst e x) = fmap Left $ unFix $ extractAll $ (afixToFix2 subst) e x

-- This might not return
normalize :: Expr -> Expr
normalize = last . smallSteps

-- All the possible steps we can take
smallSteps :: Expr -> [Expr]
smallSteps t = ana coAlg t
  where coAlg :: Expr -> ListF Expr Expr
        coAlg x = 
          case congruence x of
            (No _) -> Nil
            y -> let y' = evaluate y in Cons y' y'

-- ana that tells where to apply the substitution (SmallStep datatype)
     -- Procedure to push down the substitution
     -- repeat the procedure for Kleen closure, pushing down the previous closure
data SmallStepF f r
  = NoF (Fix f)
  | YesF (f r)
  | SubstF (Fix f) (Fix f)
  deriving (Eq, Show, Functor)

type SmallStep f = Fix (SmallStepF f)

pattern No :: Fix f -> SmallStep f
pattern No x = Fix (NoF x)

pattern Yes :: f (SmallStep f) -> SmallStep f
pattern Yes x = Fix (YesF x)

pattern Subst :: Fix f -> Fix f -> SmallStep f
pattern Subst x y = Fix (SubstF x y)

noAsFalse :: SmallStep f -> Bool
noAsFalse = maybe False (const True) . noAsNothing

noAsNothing :: SmallStep f -> Maybe (SmallStep f )
noAsNothing (No _) = Nothing
noAsNothing x = Just x