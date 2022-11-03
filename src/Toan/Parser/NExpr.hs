{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Toan.Parser.Expr
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Parser.NExpr (
  PNExpr,
  PNExprF,
  -- sexprToExpr,
  lambdaKeyword
)
where

import qualified Data.Text as T
import Toan.Parser.Location
import Toan.Language.NExpr

type PNExpr = Located' NExprF
type PNExprF = LocatedF' NExprF

lambdaKeyword :: T.Text
lambdaKeyword = "lambda"

-- sexprToExpr :: PSExpr -> Either Error PExpr
-- sexprToExpr = paraT alg
--   where alg :: PSExprF (PSExpr, Expr) 
--             -> Validation [Error] PExpr
--         alg (Ann (SAtomF (TIdentifier x))) = pure $ Annotated ((fst x), (EName x))
--         alg (Annotated l (SListF [])) = left $ EmptySExpr
--         alg (Annotated l (SListF (Right lambda:args:body:[])))
--           | aFunctor lambda == aFunctor (EName lambdaKeyword)
--           = parseLambda arg body >>= pure . Annotated l
--         alg (Annotated l (SListF (xs))) = 
--           parseApp xs >>= pure . Annotated l

--         parseLambda :: Either Error PExpr -> Either Error PExpr -> Either Error (ExprF PExpr)
--         parseLambda args body =
--           let argAlg :: ListF (Either Error PExpr) (Either Error [T.Text])
--                      -> (Either Error [T.Text])
--               argAlg Nil = pure []
--               argAlg (Cons x xs) = do
--                 x' <- x
--                 case aFunctor x' of
--                   (EName x) -> (x:) <$> xs
--                   _ -> Left $ Error (Just . aAnnotation x) LambdaArgNotIdent

--               mkLambda [] = Left $ Error
--           in do
--             as <- cata argAlg args
--             unless (null as) (left $ LambdaNoArgs)
--             b <- body
--             pure (cata (\x -> if ELam as b)