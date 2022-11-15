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
  countLambdasL,
  countLambdas,
  freeVarLevelL,
  freeVarLevel
)
where

import Text.Show.Deriving (deriveShow1)
import Data.Fix (Fix(..))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Toan.Fix.Fix
import Toan.Fix.Annotate

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

countLambdasL :: (Integer, NExprF r) -> Integer
countLambdasL (i, NLamF _ _) = i + 1
countLambdasL (i, _) = i

countLambdas :: CoAlg (Annotate ((,) Integer) NExprF) (Integer, NExpr) 
countLambdas = layerToCoAlg countLambdasL

freeVarLevelL :: ((Integer, HM.HashMap Name Integer), NExprF r) -> HM.HashMap Name Integer
freeVarLevelL ((i, m), NLamF n _) = HM.insert n i m
freeVarLevelL ((_, m), _) = m

freeVarLevel :: CoAlg (Annotate ((,) (Integer, HM.HashMap Name Integer)) NExprF) 
                    ((Integer, HM.HashMap Name Integer), NExpr)
freeVarLevel = layerToCoAlgZygo countLambdasL freeVarLevelL

-- freeVarLevel :: CoAlg (Annotate ((,) (HM.HashMap Name Integer, Integer)) NExprF)
--              -> ((HM.HashMap Name Integer, Integer), NExpr)
-- freeVarLevel = annotateCoAlg2 countL freeVarL

  -- algNExprToExpr :: NExprF ((HM.HashMap Name Index, Index) -> r) 
--                -> (HM.HashMap Name Index, Index) 
--                -> ExprF r
-- algNExprToExpr (NNameF x) (m, nbOfLambdas) = 
--   case HM.lookup x m of
--     Nothing -> ENameF x
--     (Just i) -> EIndexF (nbOfLambdas - i)
-- algNExprToExpr (NLamF x next) (m, nbOfLambdas) = 
--   let n = nbOfLambdas + 1
--       r = next (HM.insert x n m, n)
--   in ELamF r
-- algNExprToExpr (NAppF next1 next2) acc = EAppF (next1 acc) (next2 acc) 