{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Toan.Error.ErrorType
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Error.ErrorType (
  ErrorType(..),
  showErrorType,
  Error(..),
  showError
)
where

import qualified Data.Text as T
import Toan.Parser.Location

data ErrorType
  -- Related to parsing
  = KeywordMisused T.Text
  | EmptySExpr
  | LambdaArgNotIdent
  | LambdaNoArgs
  | FunctionAppNoArg

  deriving (Eq)

data Error = Error (Maybe Location) ErrorType
  deriving (Eq)

showErrorType :: ErrorType -> T.Text
showErrorType (KeywordMisused x) = T.concat ["'", x, "' is a reserved keyword."]
showErrorType EmptySExpr = "Illegal empty sexpression."
showErrorType LambdaArgNotIdent = "Lambda argument is not a valid identifier."
showErrorType LambdaNoArgs = "Lambda expression must have at least one argument."
showErrorType FunctionAppNoArg = "Function call without any argument."

showError :: Error -> T.Text
showError (Error Nothing err) = showErrorType err
showError (Error (Just x) err) = T.concat [T.pack $ startPosPretty x, showErrorType err]

instance Show ErrorType where
  show = T.unpack . showErrorType

instance Show Error where
  show = T.unpack . showError