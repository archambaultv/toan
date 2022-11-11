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
  AFix,
  extractOne,
  extractAll,
  pattern Ann,
  pattern AnnF,
  cataAnn,
  paraAnn,
  functorToAFix,
  functorToAFix2
)
where

import Data.Fix (Fix(..))
import Control.Comonad (Comonad(..))
import Data.Functor.Foldable

-- We wish we could do this for a comonad w and functor f
-- type AFix w f = Fix (\x -> w (f x))
-- Basically we annotated each layer of f with the comonad w
-- For example, to add a SourcePos to a tree, we could say :
-- type WithPos = AFix (Pos,) TreeF
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

-- Ann is shorter than Annotate ;-)
pattern Ann :: w (f x) -> Annotate w f x
pattern Ann x = Annotate x
{-# COMPLETE Ann #-}

-- Now the type we really want
type AFix w f = Fix (Annotate w f)

-- Remove one layer of annotation
extractOne :: (Comonad w) => AFix w f -> f (AFix w f)
extractOne = extract . runAnnotate . unFix

-- Remove all layers of annotation
extractAll :: forall w f . (Comonad w, Functor f) => AFix w f -> Fix f
extractAll = cata alg
  where alg :: Annotate w f (Fix f) -> Fix f
        alg = Fix . extractA

-- Removes the boiler plate (Fix (Annotate ...)) in pattern matchin
pattern AnnF :: w (f (AFix w f)) -> AFix w f
pattern AnnF x = Fix (Annotate x)
{-# COMPLETE AnnF #-}

-- Removes the Annotate constructor
cataAnn :: (Functor w, Functor f) 
        => (w (f a) -> a) 
        -> AFix w f 
        -> a
cataAnn f = cata (f . runAnnotate)

-- Removes the annotate constructor
paraAnn :: (Functor w, Functor f) 
        => (w (f (AFix w f, a)) -> a)
        -> AFix w f 
        -> a
paraAnn f = para (f . runAnnotate)

functorToAFix :: (Comonad w)
                  => (forall r . f1 (t -> r) -> t -> f2 r)
                  -> w (f1 (t -> AFix w f2)) 
                  -> t 
                  -> AFix w f2
functorToAFix g x a = Fix 
                        $ Annotate 
                        $ fmap (\w -> g (extract w) a)
                        $ duplicate x

unAFix :: (Comonad w) => AFix w f -> f (AFix w f)
unAFix = extract . runAnnotate . unFix

functorToAFix2 :: (Comonad w)
                  => (forall r . f1 (t -> r) -> t -> Either (AFix w f2) (f2 r))
                  -> w (f1 (t -> AFix w f2)) 
                  -> t 
                  -> AFix w f2
functorToAFix2 g x a = Fix 
                     $ Annotate
                     $ fmap (\w -> foo $ g (extract w) a)
                     $ duplicate x
        where foo (Left y) = unAFix y
              foo (Right y) = y
              