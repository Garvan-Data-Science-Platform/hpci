{-# LANGUAGE OverloadedStrings #-}

module Integrations (integrationSpec) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import qualified Data.Text.Lazy as LazyText
import System.Directory (makeAbsolute)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Hspec

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

integrationSpec :: Spec
integrationSpec = describe "Docker Integration Tests" $ do
  before setupContainerTargets $ do

    it "succeeds for a normal slurm job" $ \(slurm, _pbs) -> do

      let cliArgs = [ "--user", targetUser slurm
			           , "--host", "127.0.0.1"
			           , "--port", targetPort slurm
			           , "--publicKey", "test_key.pub"
			           , "--privateKey", "test_key"
			           , "schedule"
			           , "--scheduler", "slurm"
			           , "--script", "ci/test_job.slurm"
			           , "--logFile", "test_job.log"
			           , "-c", "TEST_VAR1=success,TEST_VAR2=double_success"
                ]

      (jobExitCode, _, _) <- runHpci cliArgs

      jobExitCode `shouldBe` ExitSuccess

    it "succeeds for a normal pbs job" $ \(_slurm, pbs) -> do

      let cliArgs = [ "--user", targetUser pbs
			           , "--host", "127.0.0.1"
			           , "--port", targetPort pbs
			           , "--publicKey", "test_key.pub"
			           , "--privateKey", "test_key"
			           , "schedule"
			           , "--scheduler", "pbs"
			           , "--script", "ci/test_job.pbs"
			           , "--logFile", "test_job.log"
			           , "-c", "TEST_VAR1=success,TEST_VAR2=double_success"
                ]

      (jobExitCode, _, _) <- runHpci cliArgs

      jobExitCode `shouldBe` ExitSuccess

    it "crashes when there is a slurm submission error (wrong partition)" $ \(slurm, _pbs) -> do
      let cliArgs = [ "--user", targetUser slurm
			           , "--host", "127.0.0.1"
			           , "--port", targetPort slurm
			           , "--publicKey", "test_key.pub"
			           , "--privateKey", "test_key"
			           , "schedule"
			           , "--scheduler", "slurm"
			           , "--script", "ci/test_fail.slurm"
			           , "--logFile", "test_job.log"
			           , "-c", "TEST_VAR1=success,TEST_VAR2=double_success"
                ]

      (jobExitCode, _, _) <- runHpci cliArgs

      jobExitCode `shouldBe` ExitFailure 1

    it "crashes when there is a slurm submission error (non-zero exit)" $ \(slurm, _pbs) -> do
      let cliArgs = [ "--user", targetUser slurm
			           , "--host", "127.0.0.1"
			           , "--port", targetPort slurm
			           , "--publicKey", "test_key.pub"
			           , "--privateKey", "test_key"
			           , "schedule"
			           , "--scheduler", "slurm"
			           , "--script", "ci/test_exit.slurm"
			           , "--logFile", "test_job.log"
			           , "-c", "TEST_VAR1=success,TEST_VAR2=double_success"
                ]

      (jobExitCode, _, _) <- runHpci cliArgs

      jobExitCode `shouldBe` ExitFailure 7

    it "crashes when there is a pbs job submission error" $ \(_slurm, pbs) -> do
      let cliArgs = [ "--user", targetUser pbs
			           , "--host", "127.0.0.1"
			           , "--port", targetPort pbs
			           , "--publicKey", "test_key.pub"
			           , "--privateKey", "test_key"
			           , "schedule"
			           , "--scheduler", "pbs"
			           , "--script", "ci/test_fail.pbs"
			           , "--logFile", "test_job.log"
			           , "-c", "TEST_VAR1=success,TEST_VAR2=double_success"
                ]

      (jobExitCode, _, _) <- runHpci cliArgs

      jobExitCode `shouldBe` ExitFailure 1

    it "succeeds when a '--scheduler-arg' successfully overrides a slurm submission error" $ \(slurm, _pbs) -> do
      let cliArgs = [ "--user", targetUser slurm
			           , "--host", "127.0.0.1"
			           , "--port", targetPort slurm
			           , "--publicKey", "test_key.pub"
			           , "--privateKey", "test_key"
			           , "schedule"
			           , "--scheduler", "slurm"
			           , "--script", "ci/test_fail.slurm"
			           , "--logFile", "test_job.log"
                 , "--scheduler-arg", "--partition=all"
                 , "--scheduler-arg", "--job-name=testjob"
			           , "-c", "TEST_VAR1=success,TEST_VAR2=double_success"
                ]

      (jobExitCode, _, _) <- runHpci cliArgs

      jobExitCode `shouldBe` ExitSuccess

    it "succeeds when a '--scheduler-arg' overrides a pbs job submission error" $ \(_slurm, pbs) -> do
      let cliArgs = [ "--user", targetUser pbs
			           , "--host", "127.0.0.1"
			           , "--port", targetPort pbs
			           , "--publicKey", "test_key.pub"
			           , "--privateKey", "test_key"
			           , "schedule"
			           , "--scheduler", "pbs"
			           , "--script", "ci/test_fail.pbs"
			           , "--logFile", "test_job.log"
                 , "--scheduler-arg", "-q workq"
                 , "--scheduler-arg", "-N testjob"
			           , "-c", "TEST_VAR1=success,TEST_VAR2=double_success"
                ]

      (jobExitCode, _, _) <- runHpci cliArgs

      jobExitCode `shouldBe` ExitSuccess

    it "escapes tricky characters in slurm '--scheduler-arg'" $ \(slurm, _pbs) -> do
      let cliArgs = [ "--user", targetUser slurm
			           , "--host", "127.0.0.1"
			           , "--port", targetPort slurm
			           , "--publicKey", "test_key.pub"
			           , "--privateKey", "test_key"
			           , "schedule"
			           , "--scheduler", "slurm"
			           , "--script", "ci/test_job.slurm"
			           , "--logFile", "test_job.log"
                 , "--scheduler-arg", "--job-name='Build&Test'"
			           , "-c", "TEST_VAR1=success,TEST_VAR2=double_success"
                ]

      (jobExitCode, _, _) <- runHpci cliArgs

      jobExitCode `shouldBe` ExitSuccess

    it "escapes tricky characters in pbs '--scheduler-arg'" $ \(_slurm, pbs) -> do
      let cliArgs = [ "--user", targetUser pbs
			           , "--host", "127.0.0.1"
			           , "--port", targetPort pbs
			           , "--publicKey", "test_key.pub"
			           , "--privateKey", "test_key"
			           , "schedule"
			           , "--scheduler", "pbs"
			           , "--script", "ci/test_job.pbs"
			           , "--logFile", "test_job.log"
                 , "--scheduler-arg", "-l 'walltime=01:30:00'"
			           , "-c", "TEST_VAR1=success,TEST_VAR2=double_success"
                ]

      (jobExitCode, _, _) <- runHpci cliArgs

      jobExitCode `shouldBe` ExitSuccess
