{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8

import Control.Concurrent
import Data.List (intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Exit
import System.FilePath

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2

import Cli
import Exec
import Helpers

-- Helper functions


-- Convert the map to a string of key-value pairs
mapToString :: Map.Map Text Text -> String
mapToString = intercalate "," . map (\(k, v) -> T.unpack k ++ "=" ++ T.unpack v) . Map.toList

-- Construct the qsub command with options
constructQsubCommand :: Options -> String
constructQsubCommand opts =
  let configString = mapToString (optConfig $ optCommand opts)
      configArg = if not (Map.null (optConfig $ optCommand opts)) 
                  then " -v " ++ configString 
                  else ""
  in "qsub" ++ configArg ++ " " ++ takeFileName (script $ optCommand opts)

parseSubmissionResult :: (Int, BSL.ByteString) -> String
parseSubmissionResult = BSL8.unpack . head . BSL8.split '.' . snd

-- Parses a field from typical `qstat -xf` response (example below)
--
--   ```
--   Job Id: 122066731.pbs
--       Job_Name = STDIN
--       Job_Owner = username@hpc-hostname
--       resources_used.cpupercent = 1
--       resources_used.cput = 00:00:00
--       resources_used.jobfs = 0b
--       resources_used.mem = 24408kb
--       resources_used.ncpus = 1
--       resources_used.vmem = 24408kb
--       resources_used.walltime = 00:00:31
--       job_state = F
--   ```
--   The function takes a qstat response and a key parameter. It drops the first line of the response, and attempts to parse a key-value pair from each line. If parsing was successful, tries to filter the parsed pairs by those whos key to matches the desired key
findKeyValuePair :: [T.Text] -> T.Text -> Either String (T.Text, T.Text)
findKeyValuePair pairs keyOfInterest =
    case listToMaybe [kv | Right kv@(k, _) <- map (parsePair " = ") pairs, k == keyOfInterest] of
        Just kv -> Right kv
        Nothing -> Left $ "Key " ++ T.unpack keyOfInterest ++ " not found"

checkStatus :: Session -> String -> String -> IO (Either String T.Text)
checkStatus s jid keyOfInterest = do
  jobStatus <- runCommand s ("qstat -fx " ++ jid)
  let statusLines = map (T.strip . T.pack) (tail $ lines (BSL8.unpack $ snd jobStatus))
  let result = findKeyValuePair statusLines (T.pack keyOfInterest)
  case result of
    Right (_,v) -> return $ Right v
    Left err    -> return $ Left err

pollUntilFinished :: Session -> String -> IO ()
pollUntilFinished s jid = do
  r <- checkStatus s jid "job_state"
  case r of
    Right "F" -> putStrLn ("Job " ++ jid ++ ": Finished")
    Right status -> do
      putStrLn ("Job status: " ++ T.unpack status)
      threadDelay 2000000
      pollUntilFinished s jid
    Left err -> putStrLn ("Error: " ++ err)

-- TODO: error handling for IO and parsing job id; parse status of job
-- TODO: explore Reader monad to replace global variables and thread config through code

runHpci :: Options -> IO ()
runHpci opts = do
  case optCommand opts of 
    Exec execStr -> runExec opts execStr

    _             -> do
      session <- sessionInit (host $ connectionInfo opts) (port $ connectionInfo opts)
      putStrLn "Start Session"

      -- Authenticate (Leave passphrase as empty string)
      publicKeyAuthFile session (user $ connectionInfo opts) (publicKey $ connectionInfo opts) (privateKey $ connectionInfo opts) ""
      putStrLn "Authorised"

      -- Send a file to remote host via SCP.
      scriptSize <- scpSendFile session 0o644 (script $ optCommand opts) (takeFileName $ (script $ optCommand opts))
      putStrLn $ "Sent: " ++ (script $ optCommand opts) ++ " - "++ show scriptSize ++ " bytes."

      -- Submit job using script file
      putStrLn $ "Qsub command to run on server: " ++ constructQsubCommand opts

      submissionResult <- runCommand session ((constructQsubCommand opts) ++ " 2>&1")

      let jobId = parseSubmissionResult submissionResult

      putStrLn ("Job ID: " ++ jobId)

      -- Query job status
      -- TODO: Add timeout?
      pollUntilFinished session jobId

      -- Get exit status
      exitStatus <- checkStatus session jobId "Exit_status"

      -- Copy logs file off server to ci
      logSize <- scpReceiveFile session (logFile $ optCommand opts) (logFile $ optCommand opts)
      putStrLn $ "Received: " ++ (logFile $ optCommand opts) ++ " - " ++ show logSize ++ " bytes."

      -- Remove script from server
      _ <- withChannel session $ \ch -> do
             channelExecute ch ("rm " ++ (script $ optCommand opts))
             result <- readAllChannel ch
             BSL.putStr result

      -- Close active session
      sessionClose session
      putStrLn "Closed Session"

      -- Print logs file
      contents <- readFile $ (logFile $ optCommand opts)
      putStrLn "Contents of log file:"
      putStr contents

      -- Exit with the same exit status of the HPC job (this gives us a nice CI error)
      case exitStatus of
        Left err -> putStrLn $ "WARNING: " ++ err
        Right s  -> do
          let exitCode = T.unpack s
          case exitCode of
            "0" ->  putStrLn $ "Job Exit Status: " ++ exitCode
            _   ->  do
              putStrLn $ "Job Exit Status: " ++ exitCode
              exitWith (ExitFailure $ read exitCode)

main :: IO()
main = do
  opts <- parseOptions
  runHpci opts
