
-- |
-- Module      :  Toan.Cli.Cli
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Cli.Cli
(
  cli
) where

import Toan.Cli.Command
import Options.Applicative

toanFile :: Parser String
toanFile = argument str (metavar "TOAN-FILE" <> help "The file containing the source code.")

evalC :: Parser Command
evalC = CEval
      <$> toanFile

evalInfo :: ParserInfo Command
evalInfo = info (evalC <**> helper)
              (fullDesc
               <> progDesc "Evaluate a Toan file")

parseCommand :: Parser Command
parseCommand = subparser
  ( command "eval" evalInfo
  )

opts :: ParserInfo Command
opts = info (parseCommand <**> helper)
       (fullDesc
         <> progDesc "Executes COMMAND. Use toan -h to list the \
                     \possible commands. Use toan COMMAND -h for help \
                     \on a specific command."
         <> header "toan - A toy compiler")

cli :: IO ()
cli = execParser opts >>= runCommand