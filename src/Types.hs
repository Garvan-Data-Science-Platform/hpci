module Types (
  JobId
  , getJobId
  , showJobId
  , mkJobId
) where

import Text.Read
import Data.Text (Text, pack)

-- Custom type to represent a valid Job ID
newtype JobId = JobId { getJobId :: Integer } deriving (Show, Eq)

mkJobId :: String -> Either Text JobId
mkJobId s =
  case readMaybe s of
    Just n | n >= 0 -> Right (JobId n)
    Just _          -> Left $ pack "Job ID cannot be negative."
    Nothing         -> Left $ pack ("Invalid Job ID: " ++ s)

showJobId :: JobId -> String
showJobId = show . getJobId
