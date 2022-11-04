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
  NExpr(..),
  NExprF(..)
)
where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import qualified Data.Text as T

type Name = T.Text

data NExpr
  = NName Name
  | NLam Name NExpr
  | NApp NExpr NExpr
  deriving (Show, Eq)

makeBaseFunctor ''NExpr