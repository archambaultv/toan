{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
    PatternSynonyms, ScopedTypeVariables, TupleSections, TemplateHaskell,
    ViewPatterns, RankNTypes #-}

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
  countLambdas,
  freeVars,
  nexprToExpr
)
where

import Text.Show.Deriving (deriveShow1)
import Data.Fix (Fix(..))
import Data.Set (Set)
import Control.Comonad
import Data.Functor.Foldable (ana)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Toan.Fix.Attribute
import Toan.Language.NExpr hiding (countLambdas)
import qualified Toan.Language.NExpr as N

data ExprF r
  = ENameF Name
  | EIndexF Index
  | ELamF r
  | EAppF r r
  deriving (Show, Eq, Functor, Foldable, Traversable)

$(deriveShow1 ''ExprF)

type Expr = Fix ExprF

pattern EName :: Name -> Expr
pattern EName x = Fix (ENameF x)

pattern EIndex :: Index -> Expr
pattern EIndex x = Fix (EIndexF x)

pattern ELam :: Expr -> Expr
pattern ELam x = Fix (ELamF x)

pattern EApp :: Expr -> Expr -> Expr
pattern EApp e1 e2 = Fix (EAppF e1 e2)

nexprToExpr :: forall w . (Comonad w) => AFix w NExprF -> AFix w ExprF
nexprToExpr e = ana (copyAttributeW coAlg) ((0, HM.empty), e)
  where acc :: (forall r 
               . (Index, HM.HashMap Name Index)
              -> NExprF r
              -> (Index, HM.HashMap Name Index))
        acc = joinAcc N.countLambdas freeVarLevel

        layer :: ((Index, HM.HashMap Name Index), NExprF r) -> ExprF ((Index, HM.HashMap Name Index),  r)
        layer = joinAccLayer acc coAlgNExprToExp

        coAlg :: CoAlg ExprF ((Index, HM.HashMap Name Index), AFix w NExprF)
        coAlg = layerToAFix layer

-- Count the number of lambdas
countLambdas :: Integer -> ExprF r -> Integer
countLambdas i (ELamF _) = i + 1
countLambdas i _ = i

-- Compute the set of free variables
freeVars :: Alg ExprF (Set T.Text)
freeVars (ENameF name) = S.singleton name
freeVars fVars = foldr S.union S.empty fVars

-- Translate a named expression into an expression
coAlgNExprToExp :: forall r . (Index, HM.HashMap Name Index) -> NExprF r -> ExprF r
coAlgNExprToExp (nbOfLambdas, m) (NNameF x) = 
  case HM.lookup x m of
      Nothing -> ENameF x
      (Just i) -> EIndexF (nbOfLambdas - i - 1)
coAlgNExprToExp _ (NLamF _ e) = ELamF e
coAlgNExprToExp _ (NAppF e1 e2) = EAppF e1 e2