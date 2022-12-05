{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, PatternSynonyms,
    ScopedTypeVariables, RankNTypes, TemplateHaskell, TupleSections #-}

-- |
-- Module      :  Toan.Fix.Attribute
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Fix.Attribute (
  Alg,
  CoAlg,
  CoAlgM,
  Attribute(..),
  extractA,
  extendA,
  duplicateA,
  joinA,
  joinTuple,
  AFix,
  extractOne,
  extractAll,
  pattern Ann,
  pattern AnnF,
  copyAttribute,
  copyAttributeW,
  copyAttributeWM,
  layerToFix,
  layerToFixM,
  layerToAFix,
  layerToAFixM,
  joinAcc,
  joinAccLayer,
  joinAccLayerM
)
where

import Text.Show.Deriving (deriveShow1)
import Data.Fix (Fix(..))
import Control.Comonad (Comonad(..))
import Data.Functor.Foldable

type Alg f a = f a -> a
type CoAlg f a = a -> f a
type CoAlgM f m a = a -> f (m a)

-- We wish we could do this for a comonad w and functor f
-- type AFix w f = Fix (\x -> w (f x))
-- Basically each layer of f has attributes in the form of the comonad w
-- For example, to add a SourcePos to a tree, we could say :
-- type WithPos = AFix ((,) Pos) TreeF
--    which we want equivalent to
-- type WithPos = Fix (\x -> (Pos, TreeF x))

-- But Haskell does not permit this kind of computation in types.
-- So we introduce the type Attribute to go around this limitation.
-- Type type is very similar to the CofreeF datatype.
newtype Attribute w f x = Attribute {runAttribute :: w (f x)}
  deriving (Show, Eq, Functor, Foldable, Traversable)

$(deriveShow1 ''Attribute)

-- Attribute is just a wrapper around w. So we can micmic its behavior.
-- Attribute w f x is a comonad for (f x), but I don't know how to show this
-- in Haskell typeclass Comonad
extractA :: (Comonad w) => Attribute w f x -> f x
extractA = extract . runAttribute

extendA :: (Comonad w) 
        => (Attribute w f x -> g y)
        -> (Attribute w f x)
        -> (Attribute w g y)
extendA f (Attribute x) = Attribute (extend (f . Attribute) x)

duplicateA :: (Comonad w)
           => (Attribute w f x)
           -> (Attribute w (Attribute w f) x)
duplicateA = extendA id

joinA :: (Functor w1)
      => (w1 (w2 (f x)) -> w3 (f x) )
      -> (Attribute w1 (Attribute w2 f) x) -> Attribute w3 f x
joinA join (Attribute x) = Attribute $ join (fmap runAttribute x)

joinTuple :: (Attribute ((,) a) (Attribute ((,) b) f) x) -> Attribute ((,) (a, b)) f x
joinTuple = joinA (\(a,(b,x)) -> ((a,b), x))

-- Ann is shorter than Attribute ;-)
pattern Ann :: w (f x) -> Attribute w f x
pattern Ann x = Attribute x
{-# COMPLETE Ann #-}

-- Now the type we really want
-- This type si similar to Cofree
type AFix w f = Fix (Attribute w f)

-- Remove one layer of attribute
extractOne :: (Comonad w) => AFix w f -> f (AFix w f)
extractOne = extractA . unFix

-- Remove all layers of attribute
extractAll :: forall w f . (Comonad w, Functor f) => AFix w f -> Fix f
extractAll = cata alg
  where alg :: Alg (Attribute w f) (Fix f)
        alg = Fix . extractA

-- Removes the boiler plate (Fix (Attribute ...)) in pattern matchin
pattern AnnF :: w (f (AFix w f)) -> AFix w f
pattern AnnF x = Fix (Attribute x)
{-# COMPLETE AnnF #-}

copyAttribute :: (Comonad w)
              => CoAlg h (AFix w f)
              -> CoAlg (Attribute w h) (AFix w f)
copyAttribute coAlg x =
  let h = coAlg x
  in extendA (const h) (unFix x)

copyAttributeW :: (Comonad w, Comonad w1)
              => CoAlg h (w1 (AFix w f))
              -> CoAlg (Attribute w h) (w1 (AFix w f))
copyAttributeW coAlg x =
  let h = coAlg x
  in extendA (const h) (unFix (extract x))

copyAttributeWM :: (Comonad w, Comonad w1)
              => ((w1 (AFix w f)) -> h (m (w1 (AFix w f))))
              -> ((w1 (AFix w f)) -> Attribute w h (m (w1 (AFix w f))))
copyAttributeWM coAlg x =
  let h = coAlg x
  in extendA (const h) (unFix (extract x))

layerToFix :: (Comonad w)
           => (forall r . w (f r) -> h (w r))
           -> CoAlg h (w (Fix f))
layerToFix foo = foo . fmap unFix

layerToFixM :: (Comonad w)
           => (forall r . w (f r) -> h (m (w r)))
           -> CoAlgM h m (w (Fix f))
layerToFixM foo = foo . fmap unFix

layerToAFix :: (Comonad w, Comonad w1)
             => (forall r . w1 (f r) -> h (w1 r))
             -> CoAlg h (w1 (AFix w f))
layerToAFix foo = foo . fmap extractOne

layerToAFixM :: (Comonad w, Comonad w1)
             => (forall r . w1 (f r) -> h (m (w1 r)))
             -> CoAlgM h m (w1 (AFix w f))
layerToAFixM foo = foo . fmap extractOne

-- accToLayer :: (Functor f)
--            => (forall r . a -> f r -> a) -- An accumulator that only depends on the functor, not the value inside
--            -> (forall r . (a, f r) -> f (a, r))
-- accToLayer foo (a0, f0) = fmap (foo a0 f0,) f0
  
joinAcc :: (a -> c -> a) -- An accumulator
        -> ((a, b) -> c -> b) -- Another accumulator that needs the previous accumulator value
        -> ((a, b) -> c -> (a, b))
joinAcc foo1 foo2 (a0, b0) f0 =
  let a = foo1 a0 f0
      b = foo2 (a0, b0) f0
  in (a, b)

joinAccLayer :: (Functor h)
           => (forall r . a -> f r -> a) -- Accumulator
           -> (forall r . a -> f r -> h r) -- Layer
           -> (forall r . (a, f r) -> h (a, r)) -- Layer with the accumulator
joinAccLayer acc coAlg (a0, f0) =
  let a = acc a0 f0
      h = coAlg a0 f0
  in fmap (a,) h

joinAccLayerM :: (Functor h, Functor m)
           => (forall r . a -> f r -> a) -- Accumulator
           -> (forall r . a -> f r -> h (m r)) -- Layer
           -> (forall r . (a, f r) -> h (m (a, r))) -- Layer with the accumulator
joinAccLayerM acc coAlg (a0, f0) =
  let a = acc a0 f0
      h = coAlg a0 f0
  in (fmap . fmap) (a,) h

-- joinAccSubst :: (Functor h)
--            => (forall r . a -> f r -> a) -- Accumulator
--            -> (forall r . a -> f r -> h (Bool, r)) -- Layer for substitution
--            -> (forall r . (a -> h r) -> (a, f r) -> h (Either r (a, r))) -- Layer with the accumulator
-- joinAccSubst acc layer subst (a0, f0) =
--   let a = acc a0 f0
--       h = layer subst a0 f0
--   in fmap (a,) h

-- joinAccSubstM :: (Functor h, Traversable m)
--            => (forall r . a -> f r -> a) -- Accumulator
--            -> (forall r . (a -> f r) -> a -> f r -> h (m r)) -- Layer
--            -> (forall r . (a -> f r1) -> (a, f r1) -> h (m (a, r1))) -- Layer with the accumulator
-- joinAccSubstM acc coAlg subst (a0, f0) =
--   let a = acc a0 f0
--       h = coAlg subst a0 f0
--   in (traverse . fmap) (a,) h

-- -- Like an apomorphism, but less general.
-- -- Apo is : (a -> Base t (Either t a) -> a -> t)
-- -- but with a substitution there is a link between a and t
-- subst :: (Comonad w)
--       => (w t -> Base t (Either t (w t)) -> w t -> t)
--       -> w t
--       -> t
-- subst