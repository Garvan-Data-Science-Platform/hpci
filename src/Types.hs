module Types (
  JobId
  , getJobId
  , mkJobId
) where

import Data.Char (isDigit)

-- Custom type to represent a valid Job ID
-- TODO: should this be int?
newtype JobId = JobId { getJobId :: String } deriving (Show, Eq)

-- TODO: should this be int?
mkJobId :: String -> Maybe JobId
mkJobId s
  | not (null s) && all isDigit s = Just (JobId s)
  | otherwise                     = Nothing
