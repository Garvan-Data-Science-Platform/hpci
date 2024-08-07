{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8

import Control.Concurrent
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Options.Applicative
import System.FilePath

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2

-- Data types for CLI options

data Options = Options {
  connectionInfo :: Connection,
  keys           :: KeyFiles,
  script         :: FilePath,
  logFile        :: FilePath,
  optConfig      :: KeyValuePairs
} deriving (Show)

data Connection = Connection {
  user :: String,
  host :: String,
  port     :: Int
} deriving (Show)

data KeyFiles = KeyFiles {
  publicKey  :: FilePath,
  privateKey :: FilePath
} deriving (Show)

-- Parsers for CLI options

connectionParser :: Parser Connection
connectionParser = Connection <$> userParser <*> hostParser <*> portParser
  where
    userParser = strOption (long "user" <> help "Username")
    hostParser = strOption (long "host" <> help "Hostname")
    portParser = option auto (long "port" <> help "Port number" <> metavar "INT")

keyFilesParser :: Parser KeyFiles
keyFilesParser = KeyFiles <$> publicKeyParser <*> privateKeyParser
  where
    publicKeyParser = strOption (long "publicKey" <> help "Public ssh key file path")
    privateKeyParser = strOption (long "privateKey" <> help "Private ssh key file path")

scriptParser :: Parser FilePath
scriptParser = strOption (long "script")

logFileParser :: Parser FilePath
logFileParser = strOption (long "logFile")

type KeyValuePairs = Map Text Text

keyValuePairsReader :: ReadM KeyValuePairs
keyValuePairsReader = eitherReader parseKeyValuePairs

parseKeyValuePairs :: String -> Either String KeyValuePairs
parseKeyValuePairs input = do
  let pairs = T.splitOn "," (T.pack input)
  parsedPairs <- mapM parsePair pairs
  return $ Map.fromList parsedPairs
  where
    parsePair pair = case T.splitOn "=" pair of
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
options = Options <$> connectionParser <*> keyFilesParser <*> scriptParser <*> logFileParser <*> (fromMaybe Map.empty <$> keyValuePairsOption)

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
  let configString = mapToString (optConfig opts)
      configArg = if not (Map.null (optConfig opts)) 
                  then " -v " ++ configString 
                  else ""
  in "qsub" ++ configArg ++ " " ++ takeFileName (script opts)

parseSubmissionResult :: (Int, BSL.ByteString) -> String
parseSubmissionResult = BSL8.unpack . head . BSL8.split '.' . snd

-- Parses status field from typical `qstat` response (example below)
--
--   ```
--   Job id            Name             User              Time Use S Queue
--   ----------------  ---------------- ----------------  -------- - -----
--   0.pbs_container   STDIN            pbsuser           00:00:00 R workq
--   ```
--
--   The function extracts the 2nd item of the response tuple, unpacks the Bytestring and separates it into a list of lines.
--   It takes the 3rd line from the list of lines, splits it into a list of words, then selects the 5th word.
parseQstatResponse :: (Int, BSL.ByteString) -> String
parseQstatResponse r = words (lines (BSL8.unpack $ snd r) !! 2) !! 4 

checkStatus :: Session -> String -> IO String
checkStatus s jid = do
  jobStatus <- runCommand s ("qstat -x " ++ jid)
  return (parseQstatResponse jobStatus)

pollUntilFinished :: Session -> String -> IO ()
pollUntilFinished s jid = do
  r <- checkStatus s jid
  case r of
    "F" -> putStrLn ("Job " ++ jid ++ ": Finished")
    _   -> do
      putStrLn ("Job status: " ++ r)
      threadDelay 2000000
      pollUntilFinished s jid

-- TODO: error handling for IO and parsing job id; parse status of job
-- TODO: explore Reader monad to replace global variables and thread config through code

runHpci :: Options -> IO ()
runHpci opts = do
  --  Initialize session
  session <- sessionInit (host $ connectionInfo opts) (port $ connectionInfo opts)
  putStrLn "Start Session"

  -- Authenticate (Leave passphrase as empty string)
  publicKeyAuthFile session (user $ connectionInfo opts) (publicKey $ keys opts) (privateKey $ keys opts) ""
  putStrLn "Authorised"

  -- Send a file to remote host via SCP.
  scriptSize <- scpSendFile session 0o644 (script opts) (takeFileName $ script opts)
  putStrLn $ "Sent: " ++ script opts ++ " - "++ show scriptSize ++ " bytes."

  -- Submit job using script file
  putStrLn $ "Qsub command to run on server: " ++ constructQsubCommand opts

  submissionResult <- runCommand session (constructQsubCommand opts)

  let jobId = parseSubmissionResult submissionResult

  putStrLn ("Job ID: " ++ jobId)

  -- Query job status
  -- TODO: Add timeout?
  pollUntilFinished session jobId

  -- Copy logs file off server to ci
  logSize <- scpReceiveFile session (logFile opts) (logFile opts)
  putStrLn $ "Received: " ++ logFile opts ++ " - " ++ show logSize ++ " bytes."

  -- Remove script from server
  _ <- withChannel session $ \ch -> do
         channelExecute ch ("rm " ++ script opts)
         result <- readAllChannel ch
         BSL.putStr result

  -- Close active session
  sessionClose session
  putStrLn "Closed Session"

  -- Print logs file
  contents <- readFile $ logFile opts
  putStrLn "Contents of log file:"
  putStr contents


main :: IO()
main = do
  runHpci =<< execParser opts
    where
      opts = info (options <**> helper)
        ( fullDesc
        <> progDesc "HPC in CI"
        <> header "Transfer files and execute commands on HPC via ssh")
