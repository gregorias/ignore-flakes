-- | A data structure representing a command to run.
module Command (
  Command (..),
  runCommandStrict,
) where

import Data.Text (Text)
import Turtle (ExitCode, proc, stdin)

-- | A command-line command to run
data Command = Command
  { commandCmd :: !Text
  , commandArgs :: ![Text]
  }
  deriving stock (Eq, Show)

-- | Runs the command strictly and returns its exit code.
runCommandStrict :: Command -> IO ExitCode
runCommandStrict (Command{commandCmd = commandCmd', commandArgs = commandArgs'}) =
  proc commandCmd' commandArgs' Turtle.stdin
