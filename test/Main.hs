module Main (main) where

import Relude
import qualified Test.Hspec as Hspec
import qualified Test.Lib

main :: IO ()
main = Hspec.hspec tests

tests :: Hspec.SpecWith ()
tests = do
  Test.Lib.tests
