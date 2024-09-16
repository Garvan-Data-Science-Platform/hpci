module Exec (runExec) where

-- TODO: add helpers iwth run command
-- import Helpers

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2
import System.Exit

import Cli
import Helpers

runExec :: Options -> String -> IO()
runExec opts execStr = do
      session <- sessionInit (host $ connectionInfo opts) (port $ connectionInfo opts)

      -- Authenticate (Leave passphrase as empty string)
      publicKeyAuthFile session (user $ connectionInfo opts) (publicKey $ connectionInfo opts) (privateKey $ connectionInfo opts) ""

      -- Run exec command
      execResult <- runCommand session (execStr ++ " 2>&1")
      let (exitCode, execOutput) = parseExecResult execResult
      putStrLn execOutput

      -- Close active session
      sessionClose session

      case exitCode of
        0 ->  exitSuccess
        _   ->  do
          putStrLn $ "Job Exit Status: " ++ (show $ exitCode)
          exitWith (ExitFailure exitCode)
