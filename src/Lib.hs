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
  long,
  metavar,
  progDesc,
  str,
  strOption,
 )
import Relude

data Program = Program
  { markfile :: !FilePath
  , cmdArgs :: [String]
  }
  deriving stock (Eq, Show)

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
        <> header "ignore-flakes -- ignore flakes"
    )

exec :: Program -> IO ()
exec (Program{markfile = _, cmdArgs = cmdArgs'}) = print cmdArgs'

programP :: Parser Program
programP =
  Program
    <$> markfileP
    <*> some (argument str (metavar "CMD..."))

markfileP :: Parser FilePath
markfileP = strOption (long "markfile" <> metavar "FILENAME")
