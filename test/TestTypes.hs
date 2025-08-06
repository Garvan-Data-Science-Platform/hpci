module TestTypes (
  MalformedInput(..)
) where

import qualified Data.ByteString.Lazy as BSL

newtype MalformedInput = MalformedInput { getMalformedBS :: BSL.ByteString } deriving Show

