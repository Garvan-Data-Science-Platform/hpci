{-# LANGUAGE OverloadedStrings #-}

module Integrations (integrationSpec) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (pack, unpack, isInfixOf)
import qualified Data.Text.Lazy as LazyText
import System.Directory (makeAbsolute)
import System.Process (readProcess)
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

integrationSpec :: Spec
integrationSpec = describe "Docker Integration Tests" $ do
  around (withContainers setupContainers) $ do

    it "has the correct version of Slurm installed" $ \(slurm, _pbs) -> do
      output <- runInContainer slurm ["sinfo", "--version"]
      pack output `shouldSatisfy` ("slurm" `isInfixOf`)

    it "has the correct version of PBS installed" $ \(_slurm, pbs) -> do
      output <- runInContainer pbs ["pbsnodes", "--version"]
      pack output `shouldSatisfy` ("version" `isInfixOf`)
