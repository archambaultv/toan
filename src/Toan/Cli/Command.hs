
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
import Control.Monad.Except (runExceptT, ExceptT(..), lift, liftEither)
import qualified Text.Megaparsec as M
import qualified Data.Text as T
import qualified Toan.Parser as P
import Toan.Language.Expr (Expr, nexprToExpr)
import Toan.Error
import Toan.Fix.Attribute (extractAll)

-- | The commands accepted by the command line interface
data Command = CEval FilePath
             deriving (Show, Eq)

-- | How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand c = runExceptT (runCommand' c) >>= either (putStrLn . T.unpack . showErrors) return

runCommand' :: Command -> ExceptT [Error] IO ()
runCommand' (CEval inputPath) = do
  sexp <- decodeFile inputPath
  nexp <- liftEither $ P.sexprToExpr sexp
  let exp1 = nexprToExpr nexp
  lift $ putStrLn (show $ (extractAll exp1 :: Expr))
  -- lift $ putStrLn (showFix showExprF $ (extractAll exp1 :: Expr))

-- Decode file as an SExpr
decodeFile :: FilePath -> ExceptT [Error] IO P.PSExpr
decodeFile inputPath = ExceptT (do
  input <- readFile inputPath
  case M.runParser (P.decodeOne :: M.Parsec Void String P.PSExpr) inputPath input of
    Left err -> return 
              $ Left 
              $ (:[])
              $ Error Nothing 
              $ Megaparsec
              $ T.pack 
              $ M.errorBundlePretty err
    Right sexp -> return $ Right sexp
  )
                 