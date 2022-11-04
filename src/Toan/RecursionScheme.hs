{-# Language FlexibleContexts, ScopedTypeVariables, TupleSections #-}
-- |
-- Module      :  Toan.RecursionScheme
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.RecursionScheme (
  cataT,
  cataM,
  paraT,
  paraM
)
where

import Data.Functor.Foldable

-- Catamorphism for traversable structure and effectful actions.
-- For example, to compute with errors without having to manually handle them :
-- cataT (ListF Int Int -> Either Error Int)
cataT :: forall a f t 
        . (Applicative f, Traversable (Base t), Recursive t) 
        => (f (Base t a) -> f a) 
        -> t
        -> f a
cataT f = c
  where c :: t -> f a
        c = f . sequenceA . fmap c . project

cataM :: forall a f t 
        . (Monad f, Traversable (Base t), Recursive t) 
        => (Base t a -> f a) 
        -> t
        -> f a
cataM f = cataT (\x -> x >>= f)

-- Paramorphism for traversable structure and effectful actions
paraT :: forall a f t 
        . (Applicative f, Traversable (Base t), Recursive t) 
        => (f (Base t (t, a)) -> f a) 
        -> t
        -> f a
paraT f = p 
  where p :: t -> f a
        p x = f . traverse tr . fmap ((,) <*> p) $ project x

        tr (x, y) = (x,) <$> y

paraM :: forall a f t 
        . (Monad f, Traversable (Base t), Recursive t) 
        => (Base t (t, a) -> f a) 
        -> t
        -> f a
paraM f = paraT (\x -> x >>= f)