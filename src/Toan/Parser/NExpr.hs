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
import Toan.RecursionScheme
import Toan.Error
import Toan.Annotated
import Toan.SExpr.SExpr
import Toan.Language.NExpr
import Toan.Parser.Location
import Toan.Parser.SExpr

type PNExpr = Located' NExprF
type PNExprF = LocatedF' NExprF

lambdaKeyword :: T.Text
lambdaKeyword = "lambda"

-- Takes care of errors and location automatically
para' :: (SExprF Token (PSExpr, PNExpr) -> Validation ErrorType (NExprF PNExpr))
      -> PSExpr 
      -> Validation [Error] PNExpr
para' f = paraT f'
  where
  -- We don't use paraM because Validation doesn't have a monad instance
  f' :: Validation [Error] (PSExprF (PSExpr, PNExpr)) -> Validation [Error] PNExpr
  f' (Failure err) = Failure err
  f' (Success (AnnotatedF (l, x))) = 
    case f x of
      Failure err -> Failure [Error (Just l) err]
      Success x' -> Success $ Annotated (l, x')

sexprToExpr :: PSExpr -> Either [Error] PNExpr
sexprToExpr = validationToEither . para' alg
  where alg :: SExprF Token (PSExpr, PNExpr) -> Validation ErrorType (NExprF PNExpr)
        alg = undefined
        -- alg (SAtomF (_, TIdentifier x)) = pure $ EName x
        -- alg (SListF []) = Failure $ (Error Nothing EmptySExpr)
        -- alg (SListF [_]) = Failure $ (Error Nothing FunctionAppNoArg)
        -- alg (SListF (lambda:args:body:[]))
        --   | noAnnotation (snd lambda) == EName lambdaKeyword
        --   = parseLambda args body
        -- alg (Annotated l (SListF (xs))) = 
        --   parseApp xs >>= pure . Annotated l

        parseLambda = undefined

        -- parseLambda :: [PSExpr] -> (PSExpr, PNExpr) Either Error (ExprF PExpr)
        -- parseLambda args body =
        --   let argAlg :: ListF (Either Error PExpr) (Either Error [T.Text])
        --              -> (Either Error [T.Text])
        --       argAlg Nil = pure []
        --       argAlg (Cons x xs) = do
        --         x' <- x
        --         case aFunctor x' of
        --           (EName x) -> (x:) <$> xs
        --           _ -> Left $ Error (Just . aAnnotation x) LambdaArgNotIdent

        --       mkLambda [] = Left $ Error
        --   in do
        --     as <- cata argAlg args
        --     unless (null as) (left $ LambdaNoArgs)
        --     b <- body
        --     pure (cata (\x -> if ELam as b)