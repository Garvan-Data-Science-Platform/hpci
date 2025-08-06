module Props where

import qualified Data.ByteString.Lazy.Char8 as BSL8
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck as QC
import Types (JobId, getJobId)
import Arbitrary()

import Schedule (parseSubmissionResult)

prop_extractsJobId :: JobId -> Bool
prop_extractsJobId jobId =
  let
    parsedResult = parseSubmissionResult (0, BSL8.pack (getJobId jobId <> ".pbs"))
  in
    parsedResult == Just jobId

props :: [TestTree]
props =
  [
    testProperty "Round trip test for parseSubmissionResult" prop_extractsJobId
  ]
