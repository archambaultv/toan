{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
    PatternSynonyms, ScopedTypeVariables #-}

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
  showExprF,
  Expr,
  pattern EName,
  pattern EIndex,
  pattern ELam,
  pattern EApp,
  nexprToExpr,
  nexprToExprAnn,
  algNExprToExpr,
  algFreeVars
)
where

import Data.Fix (Fix(..))
import Data.Set (Set)
import Control.Comonad
import Data.Functor.Foldable (cata)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Toan.Fix.Annotate
import Toan.Fix.Fix
import Toan.Language.NExpr (Name, NExpr, NExprF(..))

type Index = Int

data ExprF r
  = ENameF Name
  | EIndexF Index
  | ELamF r
  | EAppF r r
  deriving (Show, Eq, Functor, Foldable, Traversable)

showExprF :: ExprF (Expr, String) -> String
showExprF (ENameF n) = "EName " ++ T.unpack n
showExprF (EIndexF i) = "EIndex " ++ show i
showExprF (ELamF (_, s)) = "ELam " ++ s
showExprF (EAppF s1 s2) = "EApp " ++ addParen s1 ++ " " ++ addParen s2

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
nexprToExpr e = cata (functorToFix algNExprToExpr) e (HM.empty, 0)

nexprToExprAnn :: (Comonad w) => AFix w NExprF -> AFix w ExprF
nexprToExprAnn e = cataAnn (functorToAFix algNExprToExpr) e (HM.empty, 0)

-- Compute the set of free variables
algFreeVars :: ExprF (Set T.Text) -> Set T.Text
algFreeVars (ENameF name) = S.singleton name
algFreeVars fVars = foldr S.union S.empty fVars

-- Translate a named expression into an expression
algNExprToExpr :: NExprF ((HM.HashMap Name Index, Index) -> r) 
               -> (HM.HashMap Name Index, Index) 
               -> ExprF r
algNExprToExpr (NNameF x) (m, nbOfLambdas) = 
  case HM.lookup x m of
    Nothing -> ENameF x
    (Just i) -> EIndexF (nbOfLambdas - i)
algNExprToExpr (NLamF x next) (m, nbOfLambdas) = 
  let n = nbOfLambdas + 1
      r = next (HM.insert x n m, n)
  in ELamF r
algNExprToExpr (NAppF next1 next2) acc = EAppF (next1 acc) (next2 acc)