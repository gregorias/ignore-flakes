module Main (main) where

import Relude
import qualified Test.Hspec as Hspec
import qualified Test.Main

main :: IO ()
main = Hspec.hspec tests

tests :: Hspec.SpecWith ()
tests = do
  Test.Main.tests
