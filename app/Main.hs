{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8

import Control.Concurrent
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Options.Applicative
import System.Exit
import System.FilePath

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2

-- Data types for CLI options

data Options = Options {
  connectionInfo :: Connection,
  optCommand     :: Command
} deriving (Show)

data Command
  = Schedule {
      script         :: FilePath,
      logFile        :: FilePath,
      optConfig      :: KeyValuePairs
    }
  | Exec String deriving Show

data Connection = Connection {
  user :: String,
  host :: String,
  port     :: Int,
  publicKey  :: FilePath,
  privateKey :: FilePath
} deriving (Show)

-- Parsers for CLI options
commandParser :: Parser Command
commandParser = hsubparser (scheduleCommand <> execCommand)

scheduleCommand :: Mod CommandFields Command
scheduleCommand =
    command
        "schedule"
        (info scheduleOptions (progDesc "Schedule a job on HPC"))

scheduleOptions :: Parser Command
scheduleOptions =
    Schedule <$> scriptParser <*> logFileParser <*> (fromMaybe Map.empty <$> keyValuePairsOption)

execCommand :: Mod CommandFields Command
execCommand =
    command
        "exec"
        (info execOptions (progDesc "Exec the thing"))

execOptions :: Parser Command
execOptions = Exec <$> strArgument (metavar "EXEC_COMMAND" <> help "Command to execute on HPC")

connectionParser :: Parser Connection
connectionParser = Connection <$> userParser <*> hostParser <*> portParser <*> publicKeyParser <*> privateKeyParser
  where
    userParser = strOption (long "user" <> help "Username")
    hostParser = strOption (long "host" <> help "Hostname")
    portParser = option auto (long "port" <> help "Port number" <> metavar "INT")
    publicKeyParser = strOption (long "publicKey" <> help "Public ssh key file path")
    privateKeyParser = strOption (long "privateKey" <> help "Private ssh key file path")

scriptParser :: Parser FilePath
scriptParser = strOption (long "script")

logFileParser :: Parser FilePath
logFileParser = strOption (long "logFile")

type KeyValuePairs = Map Text Text

keyValuePairsReader :: ReadM KeyValuePairs
keyValuePairsReader = eitherReader parseKeyValuePairs

-- Function to parse a series of comma separated key-value pairs
parseKeyValuePairs :: String -> Either String KeyValuePairs
parseKeyValuePairs input =
    let pairs = T.splitOn "," (T.pack input)
        parsedPairs = mapM (parsePair "=") pairs
    in fmap Map.fromList parsedPairs

-- Function to parse key value pairs from text where
  -- keys and values are separated by `sep`
parsePair :: String -> T.Text -> Either String (T.Text, T.Text)
parsePair sep pair = case T.splitOn (T.pack sep) pair of
    [k, v] -> Right (k, v)
    _      -> Left $ "Invalid key-value pair: " ++ T.unpack pair

keyValuePairsOption :: Parser (Maybe KeyValuePairs)
keyValuePairsOption = optional $ option keyValuePairsReader
  ( long "config"
  <> short 'c'
  <> metavar "KEY1=VALUE1,KEY2=VALUE2, ... "
  <> help "Specify configuration as key-value pairs"
  )

options :: Parser Options
options = Options <$> connectionParser <*> commandParser

-- Helper functions

runCommand :: Session -> String -> IO (Int, BSL.ByteString)
runCommand s cmd = withChannel s $ \ch -> do
  channelExecute ch cmd
  readAllChannel ch

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
    Exec execStr  -> do
      session <- sessionInit (host $ connectionInfo opts) (port $ connectionInfo opts)
      putStrLn "Start Session"

      -- Authenticate (Leave passphrase as empty string)
      publicKeyAuthFile session (user $ connectionInfo opts) (publicKey $ connectionInfo opts) (privateKey $ connectionInfo opts) ""
      putStrLn "Authorised"

      -- Remove script from server
      _ <- withChannel session $ \ch -> do
             channelExecute ch execStr
             result <- readAllChannel ch
             BSL.putStr result

      -- Close active session
      sessionClose session
      putStrLn "Closed Session"

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

      submissionResult <- runCommand session (constructQsubCommand opts)

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
  runHpci =<< execParser opts
    where
      opts = info (options <**> helper)
        ( fullDesc
        <> progDesc "HPC in CI"
        <> header "Transfer files and execute commands on HPC via ssh")
