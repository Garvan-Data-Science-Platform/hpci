{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Specs (retrySpec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Data.IORef
import Control.Exception
import Data.Typeable
import Control.Retry

-- Test retry functionality
data MockSession = MockSession deriving (Show, Eq)

data MockConnectionError = MockConnectionError deriving (Show, Typeable)
instance Exception MockConnectionError

connectWithRetry :: RetryPolicyM IO -> IO MockSession -> IO MockSession
connectWithRetry policy actionToRetry =
  recoverAll policy (\_ -> actionToRetry)

mockConnect :: IORef Int -> IO MockSession
mockConnect attemptCounter = do
  attempt <- readIORef attemptCounter
  modifyIORef' attemptCounter (+1)
  putStrLn $ "Mock connection: trying attempt #" ++ show (attempt + 1)

  if attempt < 2
    then throwIO MockConnectionError
    else return MockSession

retrySpec :: Spec
retrySpec = describe "connectWithRetry" $ do
  it "succeeds on the third attempt" $ do
    counter <- newIORef 0
    let failingAction = mockConnect counter

    let testPolicy :: RetryPolicyM IO
        testPolicy =
          limitRetries 5 <>
          constantDelay 10
    result <- connectWithRetry testPolicy failingAction
    result `shouldBe` MockSession
