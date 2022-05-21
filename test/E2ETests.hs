module E2ETests (tests) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Time (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Lib (
  Command (Command),
  LastSuccessMark (..),
  ignoreFlakes,
 )
import System.Exit (ExitCode (..))
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
 )

tests :: Spec
tests = do
  describe "ignore-flakes" $ do
    it "mark successes and lets them through" $ do
      currentTime <- getCurrentTime
      let secondAgo = addUTCTime (secondsToNominalDiffTime (-1)) currentTime
      mark <- newIORef secondAgo
      let fakeMark = LastSuccessMark (Just <$> readIORef mark) (writeIORef mark currentTime)
          noTolerance = secondsToNominalDiffTime 0

      exitCode <- ignoreFlakes fakeMark noTolerance currentTime (Command "true" [])

      markTime <- readIORef mark
      exitCode `shouldBe` ExitSuccess
      markTime `shouldBe` currentTime

    it "masks tolerated failures" $ do
      currentTime <- getCurrentTime
      let secondAgo = addUTCTime (secondsToNominalDiffTime (-1)) currentTime
      mark <- newIORef secondAgo
      let fakeMark = LastSuccessMark (Just <$> readIORef mark) (writeIORef mark currentTime)
          twoSecondTolerance = secondsToNominalDiffTime 2

      exitCode <- ignoreFlakes fakeMark twoSecondTolerance currentTime (Command "false" [])

      markTime <- readIORef mark
      exitCode `shouldBe` ExitSuccess
      markTime `shouldBe` secondAgo

    it "lets non-tolerated failures through" $ do
      currentTime <- getCurrentTime
      let secondAgo = addUTCTime (secondsToNominalDiffTime (-1)) currentTime
      mark <- newIORef secondAgo
      let fakeMark = LastSuccessMark (Just <$> readIORef mark) (writeIORef mark currentTime)
          noTolerance = secondsToNominalDiffTime 0

      exitCode <- ignoreFlakes fakeMark noTolerance currentTime (Command "false" [])

      markTime <- readIORef mark
      exitCode `shouldBe` (ExitFailure 1)
      markTime `shouldBe` secondAgo

    it "lets failures through on read mark failures" $ do
      currentTime <- getCurrentTime
      let secondAgo = addUTCTime (secondsToNominalDiffTime (-1)) currentTime
      mark <- newIORef secondAgo
      let fakeMark = LastSuccessMark (return Nothing) (writeIORef mark currentTime)
          noTolerance = secondsToNominalDiffTime 0

      exitCode <- ignoreFlakes fakeMark noTolerance currentTime (Command "false" [])

      markTime <- readIORef mark
      exitCode `shouldBe` (ExitFailure 1)
      markTime `shouldBe` secondAgo
