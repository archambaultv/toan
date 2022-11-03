{-# LANGUAGE DeriveFunctor #-}
-- |
-- Module      :  Toan.Error.Validation
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Error.Validation (
  Validation(..),
  eitherToValidation,
  validationToEither
)
where

-- Either that accumulates error
data Validation err a
  = Failure err
  | Success a
  deriving (Eq, Show, Functor)

instance Semigroup err => Applicative (Validation err) where
  pure = Success
  (Failure e1) <*> (Failure e2) = Failure (e1 <> e2)
  (Failure e) <*> (Success _) = Failure e
  (Success _) <*> (Failure e) = Failure e
  (Success f) <*> (Success x) = Success (f x)

eitherToValidation :: Either a b -> Validation a b
eitherToValidation (Left a) = Failure a
eitherToValidation (Right b) = Success b

validationToEither :: Validation a b -> Either a b
validationToEither (Failure a) = Left a
validationToEither (Success b) = Right b