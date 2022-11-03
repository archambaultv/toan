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
  paraT
)
where

import Data.Functor.Foldable

-- Catamorphism for traversable structure and effectful actions.
-- For example, to compute with errors without having to manually handle them :
-- cataT (ListF Int Int -> Either Error Int)
cataT :: forall a f t 
        . (Monad f, Traversable (Base t), Recursive t) 
        => (Base t a -> f a) 
        -> t
        -> f a
cataT f = c
  where c :: t -> f a
        c = (\x -> sequenceA x >>= f) . fmap c . project

-- Paramorphism for traversable structure and effectful actions
paraT :: forall a f t 
        . (Monad f, Traversable (Base t), Recursive t) 
        => (Base t (t, a) -> f a) 
        -> t
        -> f a
paraT f = p 
  where p :: t -> f a
        p x = (\z -> traverse tr z >>= f) . fmap ((,) <*> p) $ project x

        tr (x, y) = (x,) <$> y