module Main (
  main
) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)
import Props (props)
import Specs (retrySpec)

main :: IO ()
main = do
  specs <- concat <$> traverse testSpecs [retrySpec]
  defaultMain $
    testGroup "All `hpci` tests"
      [
        testGroup "Props" props
      , testGroup "Retry logic" specs
      ]
