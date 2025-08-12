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

-- Test parseSubmissionResult
prop_extractsJobId :: JobId -> Bool
prop_extractsJobId jobId =
  let
    parsedResult = parseSubmissionResult (0, BSL8.pack (showJobId jobId <> ".pbs"))
  in
    parsedResult == Just jobId

prop_JobId_all_digits :: MalformedInput -> Bool
prop_JobId_all_digits malformedInput =
  let
    testInput = (0, getMalformedBS malformedInput)

    result = parseSubmissionResult testInput
  in
    result == Nothing

props :: [TestTree]
props =
  [
    testProperty "Round trip test for parseSubmissionResult" prop_extractsJobId
  , testProperty "Correct rejects malformed JobId" prop_JobId_all_digits
  ]
