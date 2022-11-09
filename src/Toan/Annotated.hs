{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
    TypeFamilies, ScopedTypeVariables, UndecidableInstances, AllowAmbiguousTypes,
    FlexibleInstances #-}

-- |
-- Module      :  Toan.Annotated
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Annotated (
  Annotated(..),
  annotation,
  aFunctor,
  AnnotatedF(..),
  mapBaseFunctor,
  mapAnnotation,
  noAnnotation
)
where

import Data.Bifunctor
import Data.Functor.Foldable

-- Functors with annotations
newtype Annotated a f = Annotated (a, f (Annotated a f))

annotation :: Annotated a f -> a
annotation (Annotated (a,_)) = a

aFunctor :: Annotated a f -> f (Annotated a f)
aFunctor (Annotated (_,x)) = x

newtype AnnotatedF a f r = AnnotatedF (a, f r)
 deriving (Show, Functor, Foldable, Traversable)

type instance Base (Annotated a f) = AnnotatedF a f

instance (Functor f) => Recursive (Annotated a f) where
  project (Annotated (a, x)) = AnnotatedF (a, x)

instance (Functor f) => Corecursive (Annotated a f) where
  embed (AnnotatedF (a, x)) = Annotated (a, x)

showA :: forall a f
        . (Functor f, Show a, Show (f String))
        => (Annotated a f) 
        -> String
showA = cata go
  where go :: AnnotatedF a f String -> String
        go (AnnotatedF (a, x)) = "Annotated (" ++ show a ++ ") ("++ show x ++ ")"

instance (Show a, Functor f, Show (f String)) => Show (Annotated a f)  where
  show = showA

mapBaseFunctor :: forall a b c f 
                . (Bifunctor f, Functor (f b))
                => (b -> c) 
                -> (Annotated a (f b)) 
                -> (Annotated a (f c))
mapBaseFunctor foo = cata go
  where go :: AnnotatedF a (f b) (Annotated a (f c)) -> (Annotated a (f c))
        go (AnnotatedF (a, x)) = Annotated (a, (first foo x))

mapAnnotation :: forall a1 a2 c f 
              . (Functor (f c))
              => (a1 -> a2) 
              -> (Annotated a1 (f c)) 
              -> (Annotated a2 (f c))
mapAnnotation foo = cata go
  where go :: AnnotatedF a1 (f c) (Annotated a2 (f c)) -> (Annotated a2 (f c))
        go (AnnotatedF (a, x)) = Annotated ((foo a), x)

noAnnotation :: forall a f
              . (Corecursive f)
              => Annotated a (Base f) -> f
noAnnotation = cata go
  where go :: AnnotatedF a (Base f) f -> f
        go (AnnotatedF (_,x)) = embed x