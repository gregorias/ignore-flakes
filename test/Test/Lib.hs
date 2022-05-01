module Test.Lib (
  tests,
) where

import Lib (
  Program (..),
  programInfo,
 )
import Options.Applicative (
  ParserResult (..),
  execParserPure,
  getParseResult,
  prefs,
 )
import Relude
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: Hspec.SpecWith ()
tests = do
  Hspec.describe "Lib" $ do
    Hspec.describe "programInfo" $ do
      Hspec.it "parses a command to run" $ do
        getParseResult
          ( execParser
              [ "--markfile=/var/spool/last-job.txt"
              , "--"
              , "hello"
              , "--world"
              ]
          )
          `shouldBe` Just
            ( Program
                { markfile = "/var/spool/last-job.txt"
                , cmdArgs = ["hello", "--world"]
                }
            )

execParser :: [String] -> ParserResult Program
execParser = execParserPure emptyPrefs programInfo
 where
  emptyPrefs = prefs mempty
