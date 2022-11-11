{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, PatternSynonyms,
    ScopedTypeVariables, RankNTypes #-}

-- |
-- Module      :  Toan.Fix.Annotate
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Fix.Annotate (
  Annotate(..),
  extractA,
  extendA,
  duplicateA,
  AnnotateFix,
  extractOne,
  extractAll,
  pattern Ann,
  pattern AnnF,
  cataAnn,
  paraAnn,
  functorToAnnotateFix
)
where

import Data.Fix (Fix(..))
import Control.Comonad (Comonad(..))
import Data.Functor.Foldable

-- We wish we could do this for a comonad w and functor f
-- type AnnotateFix w f = Fix (\x -> w (f x))
-- Basically we annotated each layer of f with the comonad w
-- For example, to add a SourcePos to a tree, we could say :
-- type WithPos = AnnotateFix (Pos,) TreeF
--    which we want equivalent to
-- type WithPos = Fix (\x -> (Pos, TreeF x))

-- But Haskell does not permit this kind of computation in types.
-- So we introduce the type Annotate to go around this limitation.
newtype Annotate w f x = Annotate {runAnnotate :: w (f x)}
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- Annotate is just a wrapper around w. So we can micmic its behavior.
-- Annotate w f x is a comonad for (f x), but I don't know how to show this
-- in Haskell typeclass Comonad
extractA :: (Comonad w) => Annotate w f x -> f x
extractA = extract . runAnnotate

extendA :: (Comonad w) 
        => (Annotate w f x -> g y)
        -> (Annotate w f x)
        -> (Annotate w g y)
extendA f (Annotate x) = Annotate (extend (f . Annotate) x)

duplicateA :: (Comonad w)
           => (Annotate w f x)
           -> (Annotate w (Annotate w f) x)
duplicateA = extendA id

pattern Ann :: w (f x) -> Annotate w f x
pattern Ann x = Annotate x
{-# COMPLETE Ann #-}

-- Now the type we really want
type AnnotateFix w f = Fix (Annotate w f)

-- Remove one layer of annotation
extractOne :: (Comonad w) => AnnotateFix w f -> f (AnnotateFix w f)
extractOne = extract . runAnnotate . unFix

-- Remove all layers of annotation
extractAll :: forall w f . (Comonad w, Functor f) => AnnotateFix w f -> Fix f
extractAll = cata alg
  where alg :: Annotate w f (Fix f) -> Fix f
        alg = Fix . extractA

pattern AnnF :: w (f (AnnotateFix w f)) -> AnnotateFix w f
pattern AnnF x = Fix (Annotate x)
{-# COMPLETE AnnF #-}

-- Removes the Annotate constructor
cataAnn :: (Functor w, Functor f) 
        => (w (f a) -> a) 
        -> AnnotateFix w f 
        -> a
cataAnn f = cata (f . runAnnotate)

-- Removes the annotate constructor
paraAnn :: (Functor w, Functor f) 
        => (w (f (AnnotateFix w f, a)) -> a)
        -> AnnotateFix w f 
        -> a
paraAnn f = para (f . runAnnotate)

functorToAnnotateFix :: (Comonad w)
                  => (forall r . f1 (t -> r) -> t -> f2 r)
                  -> w (f1 (t -> AnnotateFix w f2)) 
                  -> t 
                  -> AnnotateFix w f2
functorToAnnotateFix g x a = Fix 
                        $ Annotate 
                        $ fmap (\w -> g (extract w) a)
                        $ duplicate x