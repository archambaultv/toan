{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, PatternSynonyms,
    ScopedTypeVariables, RankNTypes, TemplateHaskell, TupleSections #-}

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
  layerToCoAlg,
  layerToAFixCoAlg,
  layerToCoAlgZygo,
  functorToAFix,
  functorToAFix2
)
where

import Data.Bifunctor
import Text.Show.Deriving (deriveShow1)
import Data.Fix (Fix(..))
import Control.Comonad (Comonad(..))
import Data.Functor.Foldable
import Toan.Fix.Fix

-- We wish we could do this for a comonad w and functor f
-- type AFix w f = Fix (\x -> w (f x))
-- Basically we annotated each layer of f with the comonad w
-- For example, to add a SourcePos to a tree, we could say :
-- type WithPos = AFix ((,) Pos) TreeF
--    which we want equivalent to
-- type WithPos = Fix (\x -> (Pos, TreeF x))

-- But Haskell does not permit this kind of computation in types.
-- So we introduce the type Annotate to go around this limitation.
newtype Annotate w f x = Annotate {runAnnotate :: w (f x)}
  deriving (Show, Eq, Functor, Foldable, Traversable)

$(deriveShow1 ''Annotate)

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

-- joinA :: (forall a . w2 (w1 a) -> w3 a)
--       -> (Annotate w2 (Annotate w1 f) b)
--       -> (Annotate w3 f b)
-- joinA join (Annotate x) = Annotate (join (runAnnotate x))

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
  where alg :: Alg (Annotate w f) (Fix f)
        alg = Fix . extractA

-- Removes the boiler plate (Fix (Annotate ...)) in pattern matchin
pattern AnnF :: w (f (AFix w f)) -> AFix w f
pattern AnnF x = Fix (Annotate x)
{-# COMPLETE AnnF #-}

-- Annotates Fix f with the results of the layer function
layerToCoAlg :: forall a f
             .  (Functor f)
             => (forall r . (a, f r) -> a)
             -> CoAlg (Annotate ((,) a) f) (a, Fix f)
layerToCoAlg layerF x = 
  let a :: a
      a = layerF $ fmap unFix x
  in Annotate $ fmap (fmap (a,) . unFix) x
    -- Annotate (a0, fmap (a,) (unFix x0))

layerToCoAlg4 :: forall a f w
             .  (Functor f, Bifunctor w, Functor (w a))
             => (forall r . w a (f r) -> a)
             -> CoAlg (Annotate (w a) f) (w a (Fix f))
layerToCoAlg4 layerF w = 
  let -- Compute the next value
      a :: a
      a = layerF (fmap unFix w)

      -- The context updated with the new value
      w' :: w a (Fix f)
      w' = first (const a) w

      -- Push the new context inside the recursive Fix f
      foo :: (Fix f) -> f (w a (Fix f))
      foo f = fmap (\y -> fmap (const y) w')
            $ unFix f

  in Annotate $ fmap foo w

layerToAFixCoAlg :: forall a f w
             .  (Functor f, Comonad w)
             => (forall r . (a, f r) -> a)
             -> CoAlg (Annotate w (Annotate ((,) a) f)) (a, AFix w f)
layerToAFixCoAlg layerF (a0, x0) = 
  let x :: f (AFix w f)
      x = extract $ runAnnotate $ unFix x0
      a :: a
      a = layerF (a0, x)
  in Annotate
     $ fmap (\_ -> Annotate (a0, fmap (a,) x))
     $ duplicate (runAnnotate $ unFix x0)

-- layerToAFixCoAlg :: forall a f w
--              .  (Functor f, Comonad w)
--              => CoAlg (Annotate ((,) a) f) (a, Fix f)
--              -> CoAlg (Annotate w (Annotate ((,) a) f)) (a, AFix w f)
-- layerToAFixCoAlg coAlg (a0, x0) = 
--   let x :: Fix f
--       x = extractAll x0
--       a :: a
--       a = coAlg (a0, x)
--   in Annotate
--      $ fmap (\_ -> Annotate (a0, fmap (a,) (runAnnotate $ unFix x)))
--      $ duplicate (runAnnotate $ unFix x0)

-- Much like a zygo morphism, but for layer anamorphism
layerToCoAlgZygo :: forall a b f
                 . (Functor f)
                 => (forall r . (b, f r) -> b)
                 -> (forall r . ((b,a), f r) -> a)
                 -> CoAlg (Annotate ((,) (b,a)) f) ((b, a), Fix f)
layerToCoAlgZygo layerB layerA ((b0, a0), x0) =                  
  let b :: b
      b = layerB (b0, unFix x0)
      a :: a
      a = layerA ((b0, a0), unFix x0)
  in Annotate ((b0, a0), fmap ((b, a),) (unFix x0))

-- layerToAlg :: forall a b f
--            .  (Functor f)
--            => (forall r . (a, f r) -> b)
--            -> Alg f (a -> b) -- f (a -> b) -> a -> b
-- layerToAlg layerF x a = layerF (a, x)
  
  -- Fix $ fmap (layerF . (a,)) x

-- Keeps the context used to create the type Fix f
annotateCoAlg :: (Comonad w)
              => CoAlg f (w a)
              -> CoAlg (Annotate w f) (w a)
annotateCoAlg coAlg x = Annotate 
                      $ fmap coAlg
                      $ duplicate x

-- annotateCoAlg2 :: (Comonad w1, Comonad w2)
--                -> CoAlg f (w1 a)
--                -> CoAlg f (w2 (w1 a))
--                -> CoAlg (Annotate w2 (Annotate w1 f)) (w2 (w1 a))
-- annotateCoAlg2 coAlg1 coAlg2 x =
--   let x1 :: w1 a
--       x1 = extract x
--       r1 :: Annotate w1 f (w1 a)
--       r1 = (annotateCoAlg coAlg1) x1

--       r2 = annotateCoAlg coAlg2 (runAnnotate x)
--   in 

-- joinCoAlg :: w2 (w1 a) -> w3 a
--           -> w3 a -> w2 (w1 a)
--           -> CoAlg (Annotate w2 (Annotate w1 f)) (w2 (w1 a))
--           -> CoAlg (Annotate w3 f) (w3 a)
-- joinCoAlg join split coAlg = joinA join . coAlg . split

annotateAlg :: (w (f a) -> a)
            -> Alg (Annotate w f) a
annotateAlg alg = alg . runAnnotate

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
              