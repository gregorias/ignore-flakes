module Lib (
  main,
  programInfo,
  Program (..),
) where

import Control.Applicative (some, (<**>))
import Data.Time.Clock (
  DiffTime,
  secondsToDiffTime,
 )
import Options.Applicative (
  Parser,
  ParserInfo,
  argument,
  auto,
  execParser,
  fullDesc,
  header,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  str,
  strOption,
 )

data Program = Program
  { -- | the file storing the last successful execution
    markfile :: !FilePath
  , -- | how long this library should mask flakes
    flakeDurationTolerance :: DiffTime
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
    <*> flakeDurationToleranceP
    <*> some (argument str (metavar "CMD..."))

markfileP :: Parser FilePath
markfileP = strOption (long "markfile" <> metavar "FILENAME")

flakeDurationToleranceP :: Parser DiffTime
flakeDurationToleranceP =
  daysToDiffTime
    <$> option auto (long "flake-duration-tolerance-days" <> metavar "INT")
 where
  daysToDiffTime :: Integer -> DiffTime
  daysToDiffTime = secondsToDiffTime . (* (24 * 60 * 60))
