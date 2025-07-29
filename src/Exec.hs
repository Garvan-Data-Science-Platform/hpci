module Exec (runExec) where

-- TODO: add helpers iwth run command
-- import Helpers

import Network.SSH.Client.LibSSH2
import System.Exit

import Cli
import Helpers

runExec :: Options -> String -> IO()
runExec opts execStr = do
    let connInfo                  = connectionInfo opts

    session <- safeSessionInit (host connInfo) (port connInfo)

    -- Authenticate (Leave passphrase as empty string)
    safePublicKeyAuthFile session (user connInfo) (publicKey connInfo) (privateKey connInfo)

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
