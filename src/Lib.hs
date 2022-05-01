module Lib (
  main,
  programInfo,
  Program (..),
) where

import Options.Applicative (
  Parser,
  ParserInfo,
  argument,
  execParser,
  fullDesc,
  header,
  helper,
  info,
  metavar,
  progDesc,
  str,
 )
import Relude

newtype Program = Program
  { cmdArgs :: [String]
  }
  deriving newtype (Eq, Show)

main :: IO ()
main = do
  exec =<< execParser programInfo

-- | The optparse-applicative description of this app
programInfo :: ParserInfo Program
programInfo =
  info
    (programP <**> helper)
    ( fullDesc
        <> progDesc "Run a command but ignore flakes"
        <> header "tolerate-fresh-failures -- ignore flakes"
    )

exec :: Program -> IO ()
exec (Program cmdArgs') = print cmdArgs'

programP :: Parser Program
programP = Program <$> some (argument str (metavar "CMD..."))
