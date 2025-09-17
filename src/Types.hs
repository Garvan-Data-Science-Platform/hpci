module Types (
  JobId
  , getJobId
  , showJobId
  , mkJobId
) where

import Text.Read

-- Custom type to represent a valid Job ID
newtype JobId = JobId { getJobId :: Integer } deriving (Show, Eq)

mkJobId :: String -> Maybe JobId
mkJobId s =
  case readMaybe s of
    Nothing  -> Nothing
    Just jobInt
      | jobInt >= 0 -> Just (JobId jobInt)
      | otherwise   -> Nothing

showJobId :: JobId -> String
showJobId = show . getJobId
