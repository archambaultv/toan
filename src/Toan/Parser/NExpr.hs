{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}

-- |
-- Module      :  Toan.Parser.Expr
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Parser.NExpr (
  PNExpr,
  sexprToExpr,
  lambdaKeyword
)
where

import Data.Functor.Foldable
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Toan.Error
import Toan.Fix.Annotate
import Toan.SExpr.SExpr
import Toan.Language.NExpr
import Toan.Parser.Location
import Toan.Parser.SExpr

type PNExpr = AnnotateFix ((,) Location) NExprF

lambdaKeyword :: T.Text
lambdaKeyword = "lambda"

failure :: Location -> ErrorType -> Validation [Error] a
failure l t = Failure [errorPos l t]

sexprToExpr :: PSExpr -> Either [Error] PNExpr
sexprToExpr = validationToEither . paraAnn alg
  where alg :: (Location, SExprF Token (PSExpr, Validation [Error] PNExpr)) 
            -> Validation [Error] PNExpr
        alg (l, SAtomF (TIdentifier x)) = 
          if x == lambdaKeyword
          then failure l (KeywordMisused lambdaKeyword)
          else pure $ AnnF (l, NNameF x)
        alg (l, SListF []) = failure l EmptySExpr
        alg (l, SListF [_]) = failure l FunctionCallNoArg
        alg (l, SListF ((AnnF (_,(SAtomF x)),_):args:body:[]))
           | x == TIdentifier lambdaKeyword
          = do
            names <- parseArgs (fst args)
            body' <- snd body
            pure $ curryArgs l names body'
        alg (l, SListF (x1:x2:xs)) = do
          x1' <- snd x1
          x2' <- snd x2
          xs' <- traverse snd xs
          pure $ curryApp l x1' x2' xs'

        parseArgs :: PSExpr -> Validation [Error] (NE.NonEmpty Name)
        parseArgs (AnnF (_, (SAtomF (TIdentifier x))))= pure $ x NE.:| []
        parseArgs (AnnF (l, (SListF []))) = failure l LambdaNoArgs
        parseArgs (AnnF (_, (SListF xs))) =
          let pArg :: PSExpr -> Validation [Error] Name
              pArg (AnnF (_, (SAtomF (TIdentifier x)))) = Success x
              pArg (AnnF (l, _)) = failure l LambdaArgNotIdent
          in case traverse pArg xs of
               Failure err -> Failure err
               Success xs' -> pure $ head xs' NE.:| tail xs'

        curryApp :: Location -> PNExpr -> PNExpr -> [PNExpr] -> PNExpr
        curryApp l x1 x2 xs = 
          let go :: ListF PNExpr (PNExpr -> PNExpr) -> (PNExpr -> PNExpr)
              go Nil x = x
              go (Cons y2 next) y1 = next (AnnF (l, NAppF y1 y2))
          in cata go (x2 : xs) x1

        curryArgs :: Location -> NE.NonEmpty Name -> PNExpr -> PNExpr
        curryArgs l (x1 NE.:| xs) body = 
          let go :: ListF Name PNExpr -> PNExpr
              go Nil = body
              go (Cons x b) = AnnF (l, NLamF x b)
              body' = cata go xs
          in AnnF (l, NLamF x1 body')


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