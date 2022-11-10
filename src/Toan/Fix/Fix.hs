{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Toan.Fix.Fix
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Fix.Fix (
  showFix
)
where

import Data.Fix (Fix(..))
import Data.Functor.Foldable (cata)

showFix :: forall f . (Functor f, Show (f String)) => Fix f -> String
showFix = cata show