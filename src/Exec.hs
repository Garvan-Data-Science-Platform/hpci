module Exec (runExec) where

-- TODO: add helpers iwth run command
-- import Helpers

import Data.Text (Text)
import qualified Data.Text as T
import Network.SSH.Client.LibSSH2
import System.Exit

import Cli
import Helpers

runExec :: Options -> Text -> IO()
runExec opts execStr = do
    let connInfo                  = connectionInfo opts

    session <- sessionInit' (host connInfo) (port connInfo)

    -- Authenticate (Leave passphrase as empty string)
    publicKeyAuthFile' session (user connInfo) (publicKey connInfo) (privateKey connInfo)

    -- Run exec command
    execResult <- runCommand session (T.unpack execStr <> " 2>&1")
    let (exitCode, execOutput) = parseExecResult execResult
    putStrLn execOutput

    -- Close active session
    sessionClose session

    case exitCode of
      0 ->  exitSuccess
      _   ->  do
        putStrLn $ "Job Exit Status: " <> show exitCode
        exitWith (ExitFailure exitCode)
