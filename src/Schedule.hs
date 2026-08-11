{-# LANGUAGE OverloadedStrings #-}

module Schedule (
  runSchedule
  , parseSubmissionResult
) where

-- Import from external libraries
import Control.Concurrent
import Control.Monad (when)
import Control.Exception (throwIO)
import Data.List (intercalate)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.Directory (getFileSize)
import System.Exit
import System.FilePath
import Text.Read (readMaybe)

import Errors (HpciUserError(..))

-- Import from FFI library
import Network.SSH.Client.LibSSH2 (
  Session
  , sessionClose
  , readAllChannel
  , withChannel)
import Network.SSH.Client.LibSSH2.Foreign (
  channelExecute)

-- Import from other files in this project
import Cli
import Helpers (
  defaultRetryPolicy
  , scpSendFileRetry
  , scpReceiveFileRetry
  , runCommand
  , connectWithRetry
  )
import Types

-- Convert the map to a string of key-value pairs
mapToString :: Map.Map Text Text -> String
mapToString = intercalate "," . fmap (\(k, v) -> T.unpack k <> "=" <> T.unpack v) . Map.toList

-- Construct the job submission command with options
constructSubmissionCommand :: Scheduler -> Options -> String
constructSubmissionCommand PBS opts =
  let schedulerArgString = unwords $ schedulerArgs $ optCommand opts
      configString = mapToString (optConfig $ optCommand opts)
      configArg = if not (Map.null (optConfig $ optCommand opts))
                  then " -v " <> configString
                  else ""
      (Script scriptPath) = script $ optCommand opts
  in unwords ["qsub", schedulerArgString, configArg, takeFileName scriptPath]
constructSubmissionCommand Slurm opts =
  let schedulerArgString = unwords $ schedulerArgs $ optCommand opts
      configString = mapToString (optConfig $ optCommand opts)
      configArg = if not (Map.null (optConfig $ optCommand opts))
                  then " --export=" <> configString
                  else ""
      (Script scriptPath) = script $ optCommand opts
  in unwords ["sbatch", schedulerArgString, configArg, takeFileName scriptPath]

parseSubmissionResult :: Scheduler -> (Int, BSL.ByteString) -> Either Text Types.JobId

-- PBS returns: "12345.pbs"
parseSubmissionResult PBS (exitCode, body)
  | exitCode /= 0 = Left $ T.pack $ "Error with exit code: " <> show exitCode <> " " <> BSL8.unpack body
  | otherwise =
    case listToMaybe ( BSL8.split '.' body) of
      Nothing     -> Left "Error: cannot parse Job ID."
      Just prefix -> mkJobId $ BSL8.unpack prefix

-- Slurm returns: "Submitted batch job 12345"
parseSubmissionResult Slurm (exitCode, body)
  | exitCode /= 0 = Left . T.pack $ "Error" <> show exitCode <> ": " <> BSL8.unpack body
  | otherwise =
    case listToMaybe ( reverse . words . BSL8.unpack $ body) of
      Nothing       -> Left $ T.pack "Error: cannor parse Job ID."
      Just jobIdStr -> mkJobId jobIdStr

-- Define the different status queries
data StatusType = JobState | JobExitCode

-- Map the statusTypes for each scheduler to their specific keys
getKey :: Scheduler -> StatusType -> String
getKey PBS JobState       = "job_state"
getKey PBS JobExitCode    = "Exit_status"
getKey Slurm JobState     = "State"
getKey Slurm JobExitCode  = "ExitCode,DerivedExitCode"

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
    case listToMaybe [kv | Right kv@(k, _) <- fmap (parsePair " = ") pairs, k == keyOfInterest] of
        Just kv -> Right kv
        Nothing -> Left $ "Key " <> T.unpack keyOfInterest <> " not found"

checkStatus :: Scheduler -> Session -> JobId -> StatusType -> IO (Either String T.Text)
checkStatus PBS s jid statusType = do
  jobStatus <- runCommand s ("qstat -fx " <> showJobId jid)
  let statusLines = map (T.strip . T.pack) (tail $ lines (BSL8.unpack $ snd jobStatus))
  let result = findKeyValuePair statusLines (T.pack $ getKey PBS statusType)
  case result of
    Right (_,v) -> return $ Right v
    Left err    -> return $ Left err
checkStatus Slurm s jid statusType = do
  jobStatus <- runCommand s ("sacct -j " <> showJobId jid <> " --format=" <> getKey Slurm statusType <> " --noheader -P -X")
  let raw = T.strip . T.pack . BSL8.unpack $ snd jobStatus
  case statusType of
    JobState    -> return $ Right $ head $ T.split (== ':') raw
    JobExitCode -> return $ Right $ maxExitCode raw

maxExitCode :: T.Text -> T.Text
maxExitCode raw =
  let nums = mapMaybe (readMaybe . T.unpack) (T.split (`elem` (":|" :: String)) raw)
  in T.pack . show $ maximum (0 : nums :: [Int])

-- TODO: check if I need to also support short codes CA, CD, F, OOM, TO
isFinished :: Scheduler -> Text -> Bool
isFinished PBS "F" = True
isFinished Slurm "COMPLETED"     = True
isFinished Slurm "FAILED"        = True
isFinished Slurm "TIMEOUT"       = True
isFinished Slurm "OUT_OF_MEMORY" = True
isFinished Slurm "CANCELLED"     = True
isFinished Slurm "NODE_FAIL"     = True
isFinished _ _ = False

pollUntilFinished :: Scheduler -> Connection -> JobId -> Int -> IO ()
pollUntilFinished schedulerType connInfo jid interval = do
  session <- connectWithRetry connInfo
  r <- checkStatus schedulerType session jid JobState
  sessionClose session
  case r of
    Left err -> putStrLn ("Error: " <> err)
    Right status
      | isFinished schedulerType status -> putStrLn ("Job " <> showJobId jid <> ": Finished")
      | otherwise                       -> do
          putStrLn ("Job status: " <> T.unpack status)
          threadDelay interval
          pollUntilFinished schedulerType connInfo jid interval

runSchedule :: Options -> IO()
runSchedule opts = do
    let connInfo                  = connectionInfo opts
        cmdOpts                   = optCommand opts
        (Script scriptPath)       = script cmdOpts

    -- Check script is not empty
    size <- getFileSize scriptPath
    when (size == 0) $
      throwIO $ LocalFileError $ "The script file '" ++ scriptPath ++ "' is empty (0 bytes)."

    session <- connectWithRetry connInfo

    -- Send a file to remote host via SCP.
    scriptSize <- scpSendFileRetry defaultRetryPolicy session (script cmdOpts)

    putStrLn $ "Sent: " <> show (script cmdOpts) <> " - " <> show scriptSize <> " bytes."

    -- Submit job using script file
    putStrLn $ "Command to run on server: " <> constructSubmissionCommand (scheduler cmdOpts) opts

    submissionResult <- runCommand session (constructSubmissionCommand (scheduler cmdOpts) opts <> " 2>&1")

    let eitherJobId = parseSubmissionResult (scheduler cmdOpts) submissionResult

    case eitherJobId of
      Left err -> do
        putStrLn $ T.unpack err
        sessionClose session
        exitWith (ExitFailure 1)

      Right jobId -> do
        putStrLn ("Job ID: " <> showJobId jobId)
        sessionClose session

        -- Query job status
        -- TODO: add user defined poll interval with default
        pollUntilFinished (scheduler cmdOpts) connInfo jobId 20000000

        -- Get exit status
        wrap_up_session <- connectWithRetry connInfo

    -- Add retry? here it is retrying parsing etc
        exitStatus <- checkStatus (scheduler cmdOpts) wrap_up_session jobId JobExitCode
        -- Copy logs file off server to ci
        logSize <- scpReceiveFileRetry defaultRetryPolicy wrap_up_session (logFile cmdOpts)
        let (LogFile logPath) = logFile cmdOpts
        putStrLn $ "Received: " <> takeFileName logPath <> " - " <> show logSize <> " bytes."
        -- Remove script from server
        _ <- withChannel wrap_up_session $ \ch -> do
               channelExecute ch ("rm " <> show (script cmdOpts))
               result <- readAllChannel ch
               BSL.putStr result
        -- Close active session
        sessionClose wrap_up_session
        putStrLn "Closed Session"
        -- Print logs file
        contents <- readFile $ takeFileName logPath
        putStrLn "Contents of log file:"
        putStr contents
        -- Exit with the same exit status of the HPC job (this gives us a nice CI error)
        case exitStatus of
          Left err -> putStrLn $ "WARNING: " <> err
          Right s  -> do
            let exitCode = T.unpack s
            case exitCode of
              "0" ->  putStrLn $ "Job Exit Status: " <> exitCode
              _   ->  do
                putStrLn $ "Job Exit Status: " <> exitCode
                exitWith (ExitFailure $ read exitCode)
