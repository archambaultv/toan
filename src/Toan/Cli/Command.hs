
-- |
-- Module      :  Toan.Cli.Command
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Cli.Command
(
  Command(..),
  runCommand
) where

import Data.Void (Void)
import Control.Monad.Except (runExceptT, ExceptT(..), lift)
import qualified Text.Megaparsec as M
import qualified Toan.Parser.Parser as P

-- | The commands accepted by the command line interface
data Command = CEval FilePath
             deriving (Show, Eq)

-- | How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand c = runExceptT (runCommand' c) >>= either putStrLn return

runCommand' :: Command -> ExceptT String IO ()
runCommand' (CEval inputPath) = do
  sexp <- decodeFile inputPath
  lift $ putStrLn (show sexp)

-- Decode file as an SExpr
decodeFile :: FilePath -> ExceptT String IO P.PSExpr
decodeFile inputPath = ExceptT (do
  input <- readFile inputPath
  case M.runParser (P.decodeOne :: M.Parsec Void String P.PSExpr) inputPath input of
    Left err -> return $ Left $ M.errorBundlePretty err
    Right sexp -> return $ Right sexp
  )
                 