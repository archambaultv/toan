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
  -- nexprToExpr,
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
import Toan.Language.NExpr (Name, Index, NExpr, NExprF(..))
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

-- nexprToExpr :: NExpr -> Expr
-- nexprToExpr e = hylo (annotateAlg algNExprToExpr)
--                      NE.countLambdas
--                      ((HM.empty, 0), e)

-- nexprToExprAnn :: (Comonad w) => AFix w NExprF -> AFix w ExprF
-- nexprToExprAnn e = cata (functorToAFix algNExprToExpr . runAnnotate) e (HM.empty, 0)

-- -- Compute the set of free variables
-- freeVars :: Alg ExprF (Set T.Text)
-- freeVars (ENameF name) = S.singleton name
-- freeVars fVars = foldr S.union S.empty fVars

-- -- This CoAlg doesn't do much, it is used to generate the countLambdas coAlg
-- countL :: CoAlg ExprF (Integer, Expr)
-- countL (i,ELam x) = ELamF (i + 1, x)
-- countL (i,e) = fmap (i,) (unFix e)

-- countLambdas :: CoAlg (Annotate ((,) Integer) ExprF) (Integer, Expr) 
-- countLambdas = annotateCoAlg countL

-- algNExprToExpr ::((HM.HashMap Name Index, Index), NExprF Expr)
--                -> Expr
-- algNExprToExpr  ((m, nbOfLambdas), NNameF x) = 
--   case HM.lookup x m of
--     Nothing -> EName x
--     (Just i) -> EIndex (nbOfLambdas - i)
-- algNExprToExpr (_, NLamF _ r) = ELam r
-- algNExprToExpr (_, NAppF r1 r2) = EApp r1 r2

-- -- Translate a named expression into an expression
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