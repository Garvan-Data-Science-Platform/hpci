module Main (
  main
) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)
import Props (props)
import Specs (retrySpec)

import Helpers (defaultRetryPolicy)

main :: IO ()
main = do
  -- Note: using traverse here as I expect there will be multiple Specs
  specs <- concat <$> traverse testSpecs [retrySpec defaultRetryPolicy]
  defaultMain $
    testGroup "All `hpci` tests"
      [
        testGroup "Props" props
      , testGroup "Retry logic" specs
      ]
