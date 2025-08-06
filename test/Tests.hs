module Main (
  main
) where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Props (props)

tests :: TestTree
tests =
  testGroup "All `hpci` tests"
    [ testGroup "Props" props ]

main :: IO ()
main = do
  defaultMain tests
