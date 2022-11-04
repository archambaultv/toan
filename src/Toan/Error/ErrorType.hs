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
  errorPos,
  showError,
  showErrors
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
  | FunctionCallNoArg
  | Megaparsec T.Text

  deriving (Eq)

data Error = Error (Maybe Location) ErrorType
  deriving (Eq)

errorPos :: Location -> ErrorType -> Error
errorPos l t = Error (Just l) t

showErrorType :: ErrorType -> T.Text
showErrorType (KeywordMisused x) = T.concat ["'", x, "' is a reserved keyword."]
showErrorType EmptySExpr = "Illegal empty sexpression."
showErrorType LambdaArgNotIdent = "Lambda argument is not a valid identifier."
showErrorType LambdaNoArgs = "Lambda expression must have at least one argument."
showErrorType FunctionCallNoArg = "Function call without any argument."
showErrorType (Megaparsec x) = x

showError :: Error -> T.Text
showError (Error Nothing err) = showErrorType err
showError (Error (Just x) err) = T.concat [T.pack $ startPosPretty x, " ", showErrorType err]

showErrors :: [Error] -> T.Text
showErrors = T.intercalate "\n\n" . map showError

instance Show ErrorType where
  show = T.unpack . showErrorType

instance Show Error where
  show = T.unpack . showError