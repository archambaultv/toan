{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell, 
    PatternSynonyms, TypeFamilies, TupleSections #-}

-- |
-- Module      :  Toan.Language.NExpr
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Language.NExpr (
  Name,
  Index,
  NExpr,
  NExprF(..),
  pattern NName,
  pattern NLam,
  pattern NApp,
  countLambdas,
  freeVarLevel
)
where

import Text.Show.Deriving (deriveShow1)
import Data.Fix (Fix(..))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

type Name = T.Text
type Index = Integer

data NExprF r
  = NNameF Name
  | NLamF Name r
  | NAppF r r
  deriving (Show, Eq, Functor, Foldable, Traversable)

$(deriveShow1 ''NExprF)

type NExpr = Fix NExprF

pattern NName :: Name -> NExpr
pattern NName x = Fix (NNameF x)

pattern NLam :: Name -> NExpr -> NExpr
pattern NLam n x = Fix (NLamF n x)

pattern NApp :: NExpr -> NExpr -> NExpr
pattern NApp e1 e2 = Fix (NAppF e1 e2)

countLambdas :: Integer -> NExprF r -> Integer
countLambdas i (NLamF _ _) = i + 1
countLambdas i _ = i

freeVarLevel :: (Integer, HM.HashMap Name Integer) -> NExprF r -> HM.HashMap Name Integer
freeVarLevel (i, m) (NLamF n _) = HM.insert n i m
freeVarLevel (_, m) _ = m