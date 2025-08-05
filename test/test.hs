module Main (
  main
) where

import Test.Tasty
import Test.Tasty.Program

main :: IO ()
main = defaultMain $ testGroup "Test with dockerised OpenPBS" $ [
    testProgram "hpci" "make" ["test-bin-schedule"] Nothing
  ]
