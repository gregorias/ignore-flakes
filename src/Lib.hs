module Lib (
  main,
  ignoreFlakes,
  LastSuccessMark (..),
  Command (..),
  programInfo,
  Program (..),
) where

import Command (Command (..), runCommandStrict)
import Control.Applicative (many, (<**>))
import Data.Text.IO (hPutStrLn)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, secondsToNominalDiffTime)
import LastSuccessMark (LastSuccessMark (..), mkFilebasedMark)
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
  short,
  str,
  strOption,
 )
import System.Exit (ExitCode (..))
import System.IO (stderr)
import Turtle (FilePath, exit)

data Program = Program
  { -- | the file storing the last successful execution
    markfile :: !Turtle.FilePath
  , -- | how long this library should mask flakes
    flakeDurationTolerance :: !NominalDiffTime
  , cmd :: !Command
  }
  deriving stock (Eq, Show)

-- | The optparse-applicative description of this app
programInfo :: ParserInfo Program
programInfo =
  info
    (programP <**> helper)
    ( fullDesc
        <> progDesc "Run a command but ignore flakes"
        <> header "ignore-flakes -- ignore flakes"
    )

programP :: Parser Program
programP =
  Program
    <$> markfileP
    <*> flakeDurationToleranceP
    <*> commandP

markfileP :: Parser Turtle.FilePath
markfileP = strOption (long "markfile" <> short 'm' <> metavar "FILENAME")

flakeDurationToleranceP :: Parser NominalDiffTime
flakeDurationToleranceP =
  daysToDiffTime
    <$> option auto (long "flake-duration-tolerance-days" <> short 't' <> metavar "INT")
 where
  daysToDiffTime :: Integer -> NominalDiffTime
  daysToDiffTime = secondsToNominalDiffTime . fromInteger . (* (24 * 60 * 60))

commandP :: Parser Command
commandP = (Command <$> (argument str (metavar "CMD")) <*> many (argument str (metavar "ARGS...")))

-- | Runs the provided command, ignores its flakes, and marks successes.
ignoreFlakes :: LastSuccessMark -> NominalDiffTime -> UTCTime -> Command -> IO ExitCode
ignoreFlakes mark tolerance now cmd' = do
  exitCode <- runCommandStrict cmd'
  case exitCode of
    ExitSuccess -> do
      writeLastSuccessMark mark
      return exitCode
    _ -> do
      lastMark <- readLastSuccessMark mark
      let diff = diffUTCTime now <$> lastMark
      case diff of
        Nothing -> do
          hPutStrLn stderr "ignore-flakes: the command has failed and couldn't read its last success."
          return exitCode
        Just diff' ->
          if diff' <= tolerance
            then return ExitSuccess
            else return exitCode

exec :: Program -> IO ()
exec (Program{markfile = markfile', flakeDurationTolerance = tolerance', cmd = cmd'}) = do
  now <- getCurrentTime
  let mark = mkFilebasedMark markfile' now
  exitCode <- ignoreFlakes mark tolerance' now cmd'
  exit exitCode

main :: IO ()
main = do
  exec =<< execParser programInfo
