{-# LANGUAGE OverloadedStrings #-}

module Integrations (integrationSpec) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import qualified Data.Text.Lazy as LazyText
import System.Directory (makeAbsolute)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Hspec
import TestContainers.Hspec
import TestContainers.Docker as D

slurmContainerReq :: FilePath -> FilePath -> ContainerRequest
slurmContainerReq absCgroup absKey = containerRequest (fromBuildContext "ci/" (Just "ci/Dockerfile.slurm"))
  & setExpose [22]
  & setVolumeMounts [
    (pack absCgroup, "/etc/slurm/cgroup.conf")
  , (pack absKey, "/tmp/authorized_keys:ro")
  ]
  & setWaitingFor (waitUntilMappedPortReachable 22)

pbsContainerReq :: FilePath -> ContainerRequest
pbsContainerReq absKey = containerRequest (fromImageId "australia-southeast1-docker.pkg.dev/nci-automation/docker/pbs:latest")
  & setExpose [22]
  & setVolumeMounts [
    (pack absKey, "/tmp/authorized_keys:ro")
  ]
  & setWaitingFor (waitUntilMappedPortReachable 22 <>
        waitUntilTimeout 120 (waitForLogLine Stdout ("Restarting OpenBSD Secure Shell server" `LazyText.isInfixOf`)))

setupContainers :: TestContainer (Container, Container)
setupContainers = do
  absolutePathToCgroup <- liftIO $ makeAbsolute "ci/cgroup.conf"
  absolutePathToKey <- liftIO $ makeAbsolute "test_key.pub"

  slurm <- run (slurmContainerReq absolutePathToCgroup absolutePathToKey)
  pbs   <- run (pbsContainerReq absolutePathToKey)
  pure (slurm, pbs)

runHpci :: [String] -> IO (ExitCode, String, String)
runHpci inputArgs = readProcessWithExitCode "cabal" (["run", "hpci-exe", "--"] ++ inputArgs) ""

integrationSpec :: Spec
integrationSpec = describe "Docker Integration Tests" $ do
  aroundAll (withContainers setupContainers) $ do

    it "succeeds for a normal slurm job" $ \(slurm, _pbs) -> do
      let sshPort = show $ D.containerPort slurm 22

      let cliArgs = [ "--user", "root"
			           , "--host", "127.0.0.1"
			           , "--port", sshPort
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
      let sshPort = show $ D.containerPort pbs 22

      let cliArgs = [ "--user", "pbsuser"
			           , "--host", "127.0.0.1"
			           , "--port", sshPort
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

    it "crashes when there is a slurm submission error" $ \(slurm, _pbs) -> do
      let sshPort = show $ D.containerPort slurm 22

      let cliArgs = [ "--user", "root"
			           , "--host", "127.0.0.1"
			           , "--port", sshPort
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

    it "crashes when there is a pbs job submission error" $ \(_slurm, pbs) -> do
      let sshPort = show $ D.containerPort pbs 22

      let cliArgs = [ "--user", "pbsuser"
			           , "--host", "127.0.0.1"
			           , "--port", sshPort
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
      let sshPort = show $ D.containerPort slurm 22

      let cliArgs = [ "--user", "root"
			           , "--host", "127.0.0.1"
			           , "--port", sshPort
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
      let sshPort = show $ D.containerPort pbs 22

      let cliArgs = [ "--user", "pbsuser"
			           , "--host", "127.0.0.1"
			           , "--port", sshPort
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

    -- it "escapes tricky characters in slurm '--scheduler-arg'" $ \(slurm, _pbs) -> do
    --   let sshPort = show $ D.containerPort slurm 22

    --   let cliArgs = [ "--user", "root"
			 --           , "--host", "127.0.0.1"
			 --           , "--port", sshPort
			 --           , "--publicKey", "test_key.pub"
			 --           , "--privateKey", "test_key"
			 --           , "schedule"
			 --           , "--scheduler", "slurm"
			 --           , "--script", "ci/test_job.slurm"
			 --           , "--logFile", "test_job.log"
    --              , "--scheduler-arg", "--job-name='Build&Test'"
			 --           , "-c", "TEST_VAR1=success,TEST_VAR2=double_success"
    --             ]

    --   (jobExitCode, _, _) <- runHpci cliArgs

    --   jobExitCode `shouldBe` ExitSuccess

    it "escapes tricky characters in pbs '--scheduler-arg'" $ \(_slurm, pbs) -> do
      let sshPort = show $ D.containerPort pbs 22

      let cliArgs = [ "--user", "pbsuser"
			           , "--host", "127.0.0.1"
			           , "--port", sshPort
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
