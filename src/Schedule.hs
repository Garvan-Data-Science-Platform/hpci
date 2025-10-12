{-# LANGUAGE OverloadedStrings #-}

module Schedule (
  runSchedule
  , parseSubmissionResult
) where

-- Import from external libraries
import Control.Concurrent
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.Exit
import System.FilePath

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
  let configString = mapToString (optConfig $ optCommand opts)
      configArg = if not (Map.null (optConfig $ optCommand opts))
                  then " -v " <> configString
                  else ""
      (Script scriptPath) = script $ optCommand opts
  in "qsub" <> configArg <> " " <> takeFileName scriptPath
constructSubmissionCommand Slurm opts =
  let configString = mapToString (optConfig $ optCommand opts)
      configArg = if not (Map.null (optConfig $ optCommand opts))
                  then " --export=" <> configString
                  else ""
      (Script scriptPath) = script $ optCommand opts
  in "sbatch" <> configArg <> " " <> takeFileName scriptPath

parseSubmissionResult :: Scheduler -> (Int, BSL.ByteString) -> Maybe Types.JobId
parseSubmissionResult PBS tuple = do
  -- PBS returns: "12345.pbs"
  prefix <- listToMaybe $ BSL8.split '.' (snd tuple)
  mkJobId $ BSL8.unpack prefix
parseSubmissionResult Slurm tuple = do
  -- Slurm returns: "Submitted batch job 12345"
  jobIdStr <- listToMaybe . reverse . words . BSL8.unpack $ snd tuple
  mkJobId jobIdStr

-- Define the different status queries
data StatusType = JobState | JobExitCode

-- Map the statusTypes for each scheduler to their specific keys
getKey :: Scheduler -> StatusType -> String
getKey PBS JobState       = "job_state"
getKey PBS JobExitCode    = "Exit_status"
getKey Slurm JobState     = "State"
getKey Slurm JobExitCode  = "DerivedExitCode"

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
  let status = head $ T.split (==':') $ T.strip . T.pack . BSL8.unpack $ snd jobStatus
  return $ Right status

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

-- TODO: error handling for IO and parsing status of job
runSchedule :: Options -> IO()
runSchedule opts = do
    let connInfo                  = connectionInfo opts
        cmdOpts                   = optCommand opts

    session <- connectWithRetry connInfo

    -- Send a file to remote host via SCP.
    scriptSize <- scpSendFileRetry defaultRetryPolicy session (script cmdOpts)

    putStrLn $ "Sent: " <> show (script cmdOpts) <> " - " <> show scriptSize <> " bytes."
    -- TODO add zero script size check

    -- Submit job using script file
    putStrLn $ "Command to run on server: " <> constructSubmissionCommand (scheduler cmdOpts) opts

    -- Note: Add retry? I'm torn. I don't want to accidentally schedule multiple concurrent jobs.
    --   Will use improved error handling instead
    submissionResult <- runCommand session (constructSubmissionCommand (scheduler cmdOpts) opts <> " 2>&1")

    let maybeJobId = parseSubmissionResult (scheduler cmdOpts) submissionResult

    case maybeJobId of
      Nothing -> do
        putStrLn "Error: Could not parse a valid Job ID from the input."
        sessionClose session

      Just jobId -> do
        putStrLn ("Job ID: " <> showJobId jobId)
        sessionClose session

        -- Query job status
        -- TODO: Add timeout?
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
