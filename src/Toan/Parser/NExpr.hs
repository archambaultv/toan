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

import Data.Functor.Foldable
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
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

-- Takes care of errors automatically
-- Unwraps the Annotated constructor
para' :: ((Location, SExprF Token (PSExpr, PNExpr)) -> Validation [Error] PNExpr)
      -> PSExpr 
      -> Validation [Error] PNExpr
para' f = paraT f'
  where
  -- We don't use paraM because Validation doesn't have a monad instance
  f' :: Validation [Error] (PSExprF (PSExpr, PNExpr)) -> Validation [Error] PNExpr
  f' (Failure err) = Failure err
  f' (Success (AnnotatedF x)) = f x

sexprToExpr :: PSExpr -> Either [Error] PNExpr
sexprToExpr = validationToEither . para' alg
  where alg :: (Location, SExprF Token (PSExpr, PNExpr)) -> Validation [Error]  PNExpr
        alg (l, SAtomF (TIdentifier x)) = pure $ Annotated (l, NNameF x)
        alg (l, SListF []) = Failure [errorPos l EmptySExpr]
        alg (l, SListF [_]) = Failure [errorPos l FunctionAppNoArg]
        alg (l, SListF (lambda:args:body:[]))
          | noAnnotation (snd lambda) == NName lambdaKeyword
          = case parseArgs (fst args) of
              Failure err -> Failure err
              Success ns -> pure $ (curryArgs l) ns (snd body)
        -- alg (Annotated l (SListF (xs))) = 
        --   parseApp xs >>= pure . Annotated l

        parseArgs :: PSExpr -> Validation [Error] (NE.NonEmpty Name)
        parseArgs (Annotated (l, (SAtomF (TIdentifier x))))= pure $ x NE.:| []
        parseArgs (Annotated (l, (SListF []))) = Failure [errorPos l LambdaNoArgs] 
        parseArgs (Annotated (l, (SListF xs))) =
          let -- go :: ListF PSExpr [Name] -> [Name]
              -- go Nil = []
              -- go Cons (Annotated _ (SAtomF (TIdentifier x))) xs = x : xs
              -- go Cons (Annotated l _) = 
              parseListArgs :: [PSExpr] -> Validation [Error] [Name]
              parseListArgs = undefined
          in case parseListArgs xs of
               Failure err -> Failure err
               Success xs' -> pure $ head xs' NE.:| tail xs'

        curryArgs :: Location -> NE.NonEmpty Name -> PNExpr -> PNExpr
        curryArgs l (x1 NE.:| xs) body = 
          let go :: ListF Name PNExpr -> PNExpr
              go Nil = body
              go (Cons x b) = Annotated (l, NLamF x b)
              body' = cata go xs
          in Annotated (l, NLamF x1 body')


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