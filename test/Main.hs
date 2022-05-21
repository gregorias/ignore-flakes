module Main (main) where

import qualified E2ETests
import qualified Test.Hspec as Hspec
import qualified Test.Lib

main :: IO ()
main = Hspec.hspec $ do
  unitTests
  e2eTests

unitTests :: Hspec.Spec
unitTests = do
  Test.Lib.tests

e2eTests :: Hspec.Spec
e2eTests = do
  E2ETests.tests
