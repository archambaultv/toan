{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
    PatternSynonyms, ScopedTypeVariables, TupleSections, TemplateHaskell #-}

-- |
-- Module      :  Toan.Language.Expr
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Language.Expr (
  Name,
  Index,
  ExprF(..),
  Expr,
  pattern EName,
  pattern EIndex,
  pattern ELam,
  pattern EApp,
  nexprToExpr,
  -- nexprToExprAnn,
  -- countLambdas,
  -- freeVars
)
where

import Text.Show.Deriving (deriveShow1)
import Data.Fix (Fix(..))
import Data.Set (Set)
import Control.Comonad
import Data.Functor.Foldable (cata, ana)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Toan.Fix.Annotate
import Toan.Fix.Fix
import Toan.Language.NExpr (Name, Index, NExpr, NExprF(..), freeVarLevel)
import qualified Toan.Language.NExpr as NE

data ExprF r
  = ENameF Name
  | EIndexF Index
  | ELamF r
  | EAppF r r
  deriving (Show, Eq, Functor, Foldable, Traversable)

$(deriveShow1 ''ExprF)

addParen :: (Expr, String) -> String
addParen (_,s) = "(" ++ s ++ ")"
-- addParen (ELam _, s) = "(" ++ s ++ ")"
-- addParen (EApp _ _, s) = "(" ++ s ++ ")"
-- addParen (_, s) = s

type Expr = Fix ExprF

pattern EName :: Name -> Expr
pattern EName x = Fix (ENameF x)

pattern EIndex :: Index -> Expr
pattern EIndex x = Fix (EIndexF x)

pattern ELam :: Expr -> Expr
pattern ELam x = Fix (ELamF x)

pattern EApp :: Expr -> Expr -> Expr
pattern EApp e1 e2 = Fix (EAppF e1 e2)

nexprToExpr :: NExpr -> Expr
nexprToExpr e = ana coAlgNExprToExp (ana freeVarLevel ((0, HM.empty), e))

nexprToExprAnn :: (Comonad w) => AFix w NExprF -> AFix w ExprF
nexprToExprAnn e = ana coAlgNExprToExp' (ana freeVarLevel' ((0, HM.empty), e))

-- Compute the set of free variables
freeVars :: Alg ExprF (Set T.Text)
freeVars (ENameF name) = S.singleton name
freeVars fVars = foldr S.union S.empty fVars

-- Translate a named expression into an expression
coAlgNExprToExp :: CoAlg ExprF (AFix ((,) (Index, HM.HashMap Name Index)) NExprF )
coAlgNExprToExp (AnnF ((nbOfLambdas, m), NNameF x)) = 
  case HM.lookup x m of
    Nothing -> ENameF x
    (Just i) -> EIndexF (nbOfLambdas - i)
coAlgNExprToExp (AnnF (_, NLamF _ e)) = ELamF e
coAlgNExprToExp (AnnF (_, NAppF e1 e2)) = EAppF e1 e2 

-- NExprF ((HM.HashMap Name Index, Index) -> r) 
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