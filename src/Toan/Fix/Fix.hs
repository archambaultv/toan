{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

-- |
-- Module      :  Toan.Fix.Fix
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Fix.Fix (
  showFix,
  baseToFix
)
where

import Data.Fix (Fix(..))
import Data.Functor.Foldable (cata)

showFix :: forall f . (Functor f, Show (f String)) => Fix f -> String
showFix = cata show

-- Takes a function from the base functors f1 and f2 to the Fix f1 and Fix f2
-- types. To use with Cata for example.
baseToFix :: (forall r . f1 (t -> r) -> t -> f2 r)
          -> (f1 (t -> Fix f2) -> t -> Fix f2)
baseToFix g x acc = Fix $ g x acc