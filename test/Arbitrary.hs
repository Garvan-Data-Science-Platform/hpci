{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary () where

import qualified Data.ByteString.Lazy.Char8 as BSL8
import Test.Tasty.QuickCheck as QC
import Types (JobId, mkJobId)
import TestHelpers (
  MalformedInput(..)
  )

instance Arbitrary JobId where
  arbitrary = do
    len <- QC.choose (1, 20)
    digitString <- QC.vectorOf len (QC.elements ['0'..'9'])
    return $ case mkJobId digitString of
      Just jid -> jid
      Nothing  -> error "Test for JobId generated invalid data"

instance Arbitrary MalformedInput where
  arbitrary = do
    randomString <- QC.oneof
      [
        -- no dots, at least one non-digit
        QC.listOf1 (QC.elements (['a'..'z'] ++ ['A'..'Z']))
        ,
        return ""  -- empty String
      ]
    return (MalformedInput (BSL8.pack randomString))
