{-# LANGUAGE OverloadedStrings #-}

module Integrations (integrationSpec) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import Data.List (isInfixOf)
import qualified Data.Text.Lazy as LazyText
import System.Directory (makeAbsolute)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Hspec
import Errors (renderSshError)
import Network.SSH.Client.LibSSH2.Errors (ErrorCode(..))

data SshTarget = SshTarget
  { targetPort :: String
  , targetUser :: String
  }

setupContainerTargets :: IO (SshTarget, SshTarget)
setupContainerTargets = pure
  ( SshTarget "2223" "root"
  , SshTarget "2222" "pbsuser"
  )

runHpci :: [String] -> IO (ExitCode, String, String)
runHpci inputArgs = readProcessWithExitCode "cabal" (["run", "hpci-exe", "--"] ++ inputArgs) ""

baseScheduleArgs :: SshTarget -> String -> String -> [String] -> [String]
baseScheduleArgs target scheduler script extraArgs =
  [ "--user", targetUser target
	, "--host", "127.0.0.1"
	, "--port", targetPort target
	, "--publicKey", "test_key.pub"
	, "--privateKey", "test_key"
	, "schedule"
	, "--scheduler", scheduler
	, "--script", script
	, "--logFile", "test_job.log"
	, "-c", "TEST_VAR1=success,TEST_VAR2=double_success"
  ] ++ extraArgs

shouldExitWith :: [String] -> ExitCode -> IO()
shouldExitWith args expectedCode = do
  (actualCode, _, _) <- runHpci args
  actualCode `shouldBe` expectedCode

integrationSpec :: Spec
integrationSpec = describe "Docker Integration Tests" $ do
  before setupContainerTargets $ do

    context "Normal Job Execution" $ do
      it "succeeds for a normal slurm job" $ \(slurm, _pbs) -> do
        baseScheduleArgs slurm "slurm" "ci/test_job.slurm" [] `shouldExitWith` ExitSuccess

      it "succeeds for a normal pbs job" $ \(_slurm, pbs) -> do
        baseScheduleArgs pbs "pbs" "ci/test_job.pbs" [] `shouldExitWith` ExitSuccess

    context "Submission Error (Wrong Partition/Queue)" $ do
      it "crashes when there is a slurm submission error (wrong partition)" $ \(slurm, _pbs) -> do
        baseScheduleArgs slurm "slurm" "ci/test_fail.slurm" [] `shouldExitWith` (ExitFailure 1)

      it "crashes when there is a pbs job submission error (wrong queue)" $ \(_slurm, pbs) -> do
        baseScheduleArgs pbs "pbs" "ci/test_fail.pbs" [] `shouldExitWith` (ExitFailure 1)

    context "Submission Error (Non-zero Exit)" $ do
      it "crashes when there is a slurm submission error (non-zero exit)" $ \(slurm, _pbs) -> do
        baseScheduleArgs slurm "slurm" "ci/test_exit.slurm" [] `shouldExitWith` (ExitFailure 7)

      it "crashes when there is a pbs job submission error (non-zero exit)" $ \(_slurm, pbs) -> do
        baseScheduleArgs pbs "pbs" "ci/test_exit.pbs" [] `shouldExitWith` (ExitFailure 7)

    context "Scheduler Argument Overrides" $ do
      it "succeeds when a '--scheduler-arg' successfully overrides a slurm submission error" $ \(slurm, _pbs) -> do
        let extra = ["--scheduler-arg", "--partition=all", "--scheduler-arg", "--job-name=testjob"]
        baseScheduleArgs slurm "slurm" "ci/test_fail.slurm" extra `shouldExitWith` ExitSuccess

      it "succeeds when a '--scheduler-arg' overrides a pbs job submission error" $ \(_slurm, pbs) -> do
        let extra = ["--scheduler-arg", "-q workq", "--scheduler-arg", "-N testjob"]
        baseScheduleArgs pbs "pbs" "ci/test_fail.pbs" extra `shouldExitWith` ExitSuccess

    context "Scheduler Argument Escaping" $ do
      it "escapes tricky characters in slurm '--scheduler-arg'" $ \(slurm, _pbs) -> do
        let extra = ["--scheduler-arg", "--job-name='Build&Test'"]
        baseScheduleArgs slurm "slurm" "ci/test_job.slurm" extra `shouldExitWith` ExitSuccess

      it "escapes tricky characters in pbs '--scheduler-arg'" $ \(_slurm, pbs) -> do
        let extra = ["--scheduler-arg", "-l 'walltime=01:30:00'"]
        baseScheduleArgs pbs "pbs" "ci/test_job.pbs" extra `shouldExitWith` ExitSuccess

    context "SSH Error messages" $ do
      it "prints a verbose FILE error when SSH keys are missing" $ \(slurm, _) -> do
        let badKeyArgs =
              [ "--user", targetUser slurm
              , "--host", "127.0.0.1"
              , "--port", targetPort slurm
              , "--publicKey", "/tmp/does_not_exist.pub"
              , "--privateKey", "/tmp/does_not_exist.key"
              , "schedule"
              , "--scheduler", "slurm"
              , "--script", "ci/test_job.slurm"
              , "--logFile", "test_job.log"
              ]

        (exitCode, stdout, stderr) <- runHpci badKeyArgs

        exitCode `shouldNotBe` ExitSuccess
        (stdout ++ stderr) `shouldSatisfy` (renderSshError FILE `isInfixOf`)

      it "prints a verbose AUTHENTICATION_FAILED error when SSH keys are invalid" $ \(slurm, _) -> do
        let badKeyArgs =
              [ "--user", targetUser slurm
              , "--host", "127.0.0.1"
              , "--port", targetPort slurm
              , "--publicKey", "ci/fake_key.pub" -- file exists but is not valid key
              , "--privateKey", "ci/fake_key" -- file exists but is not valid key
              , "schedule"
              , "--scheduler", "slurm"
              , "--script", "ci/test_job.slurm"
              , "--logFile", "test_job.log"
              ]

        (exitCode, stdout, stderr) <- runHpci badKeyArgs

        exitCode `shouldNotBe` ExitSuccess
        (stdout ++ stderr) `shouldSatisfy` (renderSshError AUTHENTICATION_FAILED `isInfixOf`)

      it "prints a verbose network error when port is incorrect" $ \(slurm, _) -> do
        let badKeyArgs =
              [ "--user", targetUser slurm
              , "--host", "127.0.0.1"
              , "--port", show (read (targetPort slurm) + 100 :: Int)
              , "--publicKey", "ci/fake_key.pub" -- file exists but is not valid key
              , "--privateKey", "ci/fake_key" -- file exists but is not valid key
              , "schedule"
              , "--scheduler", "slurm"
              , "--script", "ci/test_job.slurm"
              , "--logFile", "test_job.log"
              ]

        (exitCode, stdout, stderr) <- runHpci badKeyArgs

        exitCode `shouldNotBe` ExitSuccess
        (stdout ++ stderr) `shouldSatisfy` ("Could not establish a network connection" `isInfixOf`)
        (stdout ++ stderr) `shouldSatisfy` ("Connection refused" `isInfixOf`)

      it "prints a verbose network error when host is incorrect" $ \(slurm, _) -> do
        let badKeyArgs =
              [ "--user", targetUser slurm
              , "--host", "127.0.0.2"
              , "--port", targetPort slurm
              , "--publicKey", "ci/fake_key.pub" -- file exists but is not valid key
              , "--privateKey", "ci/fake_key" -- file exists but is not valid key
              , "schedule"
              , "--scheduler", "slurm"
              , "--script", "ci/test_job.slurm"
              , "--logFile", "test_job.log"
              ]

        (exitCode, stdout, stderr) <- runHpci badKeyArgs

        exitCode `shouldNotBe` ExitSuccess
        (stdout ++ stderr) `shouldSatisfy` ("Network connection timed out" `isInfixOf`)
        (stdout ++ stderr) `shouldSatisfy` ("timeout" `isInfixOf`)

-- user error
-- host error
-- scheduler
-- logfile
-- script
