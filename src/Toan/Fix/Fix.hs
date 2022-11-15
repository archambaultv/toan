{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

-- |
-- Module      :  Toan.Fix.Fix
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Fix.Fix (
  functorToFix,
  functorToFix2,
  Alg,
  AlgW,
  CoAlg,
  MCoAlg
)
where

import Control.Comonad
import Data.Fix (Fix(..))

type Alg f a = f a -> a
type AlgW f w a = f (w a) -> a
type CoAlg f a = a -> f a
type MCoAlg f m a = a -> f (m a)

-- Takes a function from the base functors f1 and f2 to the Fix f1 and Fix f2
-- types. To use with Cata for example.
functorToFix :: (forall r . f1 (t -> r) -> t -> f2 r)
          -> (f1 (t -> Fix f2) -> t -> Fix f2)
functorToFix g x acc = Fix $ g x acc

functorToFix2 :: (forall r . f1 (t -> r) -> t -> Either (Fix f2) (f2 r))
          -> (f1 (t -> Fix f2) -> t -> Fix f2)
functorToFix2 g x acc = Fix $ foo $ g x acc
        where foo (Left y) = unFix y
              foo (Right y) = y

-- functorToFix2 :: (forall r . f1 (t -> r) -> t -> Fix f2)
--           -> (f1 (t -> Fix f2) -> t -> Fix f2)
-- functorToFix2 g x acc = g x acc

-- Takes a function from the base functors f1 and f2 to the Fix f1 and Fix f2
-- types. To use with Cata for example.
functorToFixM :: (Applicative m, Traversable f2)
              => (forall r . f1 (t -> m r) -> t -> f2 (m r))
              -> (f1 (t -> m (Fix f2)) -> t -> m (Fix f2))
functorToFixM g x acc = fmap Fix 
                      $ sequenceA
                      $ g x acc

liftM :: forall m f1 t r f2
    .  (Monad m, Comonad m, Functor f1, Functor f2)
    => (f1 (t -> r) -> t -> f2 r) 
    -> f1 (t -> m r) -> t -> f2 (m r)
liftM g x acc = 
  let x' :: f1 (t -> r)
      x' = fmap (\f -> extract . f) x
  in fmap pure $ g x' acc