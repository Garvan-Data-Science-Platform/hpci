{-# LANGUAGE OverloadedStrings #-}

module Integrations (integrationSpec) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (pack, unpack, isInfixOf)
import qualified Data.Text.Lazy as LazyText
import System.Directory (makeAbsolute)
import System.Exit (ExitCode(..))
import System.Process (readProcess, readProcessWithExitCode)
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

runInContainer :: Container -> [String] -> IO String
runInContainer container cmd = do
  let cid = unpack (containerId container)
  readProcess "docker" (["exec", cid] ++ cmd) ""

setupContainers :: TestContainer (Container, Container)
setupContainers = do
  absolutePathToCgroup <- liftIO $ makeAbsolute "ci/cgroup.conf"
  absolutePathToKey <- liftIO $ makeAbsolute "test_key.pub"

  slurm <- run (slurmContainerReq absolutePathToCgroup absolutePathToKey)
  pbs   <- run (pbsContainerReq absolutePathToKey)
  pure (slurm, pbs)

runHpci :: [String] -> IO (ExitCode, String, String)
runHpci args = readProcessWithExitCode "cabal" (["run", "hpci-exe", "--"] ++ args) ""

integrationSpec :: Spec
integrationSpec = describe "Docker Integration Tests" $ do
  around (withContainers setupContainers) $ do

    -- it "has the correct version of Slurm installed" $ \(slurm, _pbs) -> do
    --   output <- runInContainer slurm ["sinfo", "--version"]
    --   pack output `shouldSatisfy` ("slurm" `isInfixOf`)

    -- it "has the correct version of PBS installed" $ \(_slurm, pbs) -> do
    --   output <- runInContainer pbs ["pbsnodes", "--version"]
    --   pack output `shouldSatisfy` ("version" `isInfixOf`)

    it "crashes when there is a slurm submission error" $ \(slurm, _pbs) -> do
      let port = show $ D.containerPort slurm 22

      let args = [ "--user", "root"
			           , "--host", "127.0.0.1"
			           , "--port", port
			           , "--publicKey", "test_key.pub"
			           , "--privateKey", "test_key"
			           , "schedule"
			           , "--scheduler", "slurm"
			           , "--script", "ci/test_fail.slurm"
			           , "--logFile", "test_job.log"
			           , "-c", "TEST_VAR1=success,TEST_VAR2=double_success"
                ]

      (exitCode, stdout, stderr) <- runHpci args

      exitCode `shouldBe` ExitFailure 1

    it "crashes when there is a pbs job submission error" $ \(_slurm, pbs) -> do
      let port = show $ D.containerPort pbs 22

      let args = [ "--user", "pbsuser"
			           , "--host", "127.0.0.1"
			           , "--port", port
			           , "--publicKey", "test_key.pub"
			           , "--privateKey", "test_key"
			           , "schedule"
			           , "--scheduler", "pbs"
			           , "--script", "ci/test_fail.pbs"
			           , "--logFile", "test_job.log"
			           , "-c", "TEST_VAR1=success,TEST_VAR2=double_success"
                ]

      (exitCode, stdout, stderr) <- runHpci args

      exitCode `shouldBe` ExitFailure 1
