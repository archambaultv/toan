{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell, TypeFamilies #-}

-- |
-- Module      :  Toan.Language.NExpr
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Language.NExpr (
  Name,
  NExpr,
  NExprF(..)
)
where

import Data.Fix (Fix(..))
import qualified Data.Text as T

type Name = T.Text

data NExprF r
  = NNameF Name
  | NLamF Name r
  | NAppF r r
  deriving (Show, Eq, Functor, Foldable, Traversable)

type NExpr = Fix NExprF