module Props where

import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Either (isLeft)
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
    parsedResult == Right jobId

prop_JobId_all_digits :: MalformedInput -> Bool
prop_JobId_all_digits malformedInput =
  let
    testInput = (0, getMalformedBS malformedInput)

    result = parseSubmissionResult PBS testInput
  in
    isLeft result 

props :: [TestTree]
props =
  [
    testProperty "Round trip test for parseSubmissionResult" prop_extractsJobId
  , testProperty "Correct rejects malformed JobId" prop_JobId_all_digits
  ]
