module Test.Lib (
  tests,
) where

import Data.Time.Clock (
  NominalDiffTime,
  secondsToNominalDiffTime,
 )
import Lib (
  Command (Command),
  Program (..),
  programInfo,
 )
import Options.Applicative (
  ParserResult (..),
  execParserPure,
  getParseResult,
  prefs,
 )
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: Hspec.SpecWith ()
tests = do
  Hspec.describe "Lib" $ do
    Hspec.describe "programInfo" $ do
      Hspec.it "parses a command to run" $ do
        let (twoDays :: NominalDiffTime) = secondsToNominalDiffTime $ fromInteger (2 * 24 * 60 * 60)
        getParseResult
          ( execParser
              [ "--markfile=/var/spool/last-job.txt"
              , "--flake-duration-tolerance-days=2"
              , "--"
              , "hello"
              , "--world"
              ]
          )
          `shouldBe` Just
            ( Program
                { markfile = "/var/spool/last-job.txt"
                , flakeDurationTolerance = twoDays
                , cmd = Command "hello" ["--world"]
                }
            )

execParser :: [String] -> ParserResult Program
execParser = execParserPure emptyPrefs programInfo
 where
  emptyPrefs = prefs mempty
