-- | Interface for persisting a command's last success time.
module LastSuccessMark (
  LastSuccessMark (..),
  mkFilebasedMark,
) where

import Control.Applicative (empty)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Maybe (fromJust, listToMaybe)
import Data.String (IsString (fromString))
import Data.Text (unpack)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (Format (formatShowM), ISO8601 (iso8601Format), formatReadP)
import Text.ParserCombinators.ReadP (readP_to_S)
import qualified Turtle

-- | The interface for managing a specific mark.
--
-- I didn't bother making it non-IO, because I don't think I need that much
-- reusability in the ignore-flakes context. IO is good enough.
data LastSuccessMark = LastSuccessMark
  { readLastSuccessMark :: IO (Maybe UTCTime)
  , writeLastSuccessMark :: IO ()
  -- Do not use (UTCTime -> IO ()), because I intend to create instances which
  -- have a hardcoded success time value. This makes this interface simpler,
  -- and I don't think a generic type is needed. This tool is intended to run
  -- once and for short utilities.
  }

-- | A LastSuccessMark that uses a file to persist the mark.
mkFilebasedMark :: Turtle.FilePath -> UTCTime -> LastSuccessMark
mkFilebasedMark base writeTime = LastSuccessMark readMark writeMark
 where
  -- use an unsafe fromJust, because it's a runtime error if the format doesn't
  -- work that shouldn't happen.
  formattedWriteTime = fromString . fromJust $ formatShowM iso8601Format writeTime
  readMark = runMaybeT $ do
    contents <- foo (try @SomeException (Turtle.readTextFile base))
    let utcTimeP = formatReadP iso8601Format
        readS = readP_to_S utcTimeP (unpack contents)
    (utcTime, _) <- maybe empty return (listToMaybe readS)
    return utcTime
   where
    foo :: IO (Either e a) -> MaybeT IO a
    foo ioeea = do
      aOrE <- liftIO ioeea
      either
        (const empty)
        return
        aOrE

  writeMark = do
    Turtle.writeTextFile base formattedWriteTime
