module Props where

import qualified Data.ByteString.Lazy.Char8 as BSL8
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck as QC

import Types (JobId, showJobId)
import TestHelpers (
  MalformedInput(..)
  )
import Arbitrary()

import Schedule (parseSubmissionResult)
import Cli

-- Test parseSubmissionResult
prop_extractsJobId :: JobId -> Bool
prop_extractsJobId jobId =
  let
    parsedResult = parseSubmissionResult PBS (0, BSL8.pack (showJobId jobId <> ".pbs"))
  in
  -- Change to be Right jobId
    parsedResult == Just jobId

prop_JobId_all_digits :: MalformedInput -> Bool
prop_JobId_all_digits malformedInput =
  let
    testInput = (0, getMalformedBS malformedInput)

    result = parseSubmissionResult PBS testInput
  in
  -- Change to be Left with error message
    result == Nothing

props :: [TestTree]
props =
  [
    testProperty "Round trip test for parseSubmissionResult" prop_extractsJobId
  , testProperty "Correct rejects malformed JobId" prop_JobId_all_digits
  ]

  -- ADD integration test with bad jobID and non-zero exit status code
