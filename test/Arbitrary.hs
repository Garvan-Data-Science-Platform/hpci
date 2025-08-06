{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary () where

import Test.Tasty.QuickCheck as QC
import Types (JobId, mkJobId)

instance Arbitrary JobId where
  arbitrary = do
    len <- QC.choose (1, 20)
    digitString <- QC.vectorOf len (QC.elements ['0'..'9'])
    return $ case mkJobId digitString of
      Just jid -> jid
      Nothing  -> error "Test for JobId generated invalid data"

