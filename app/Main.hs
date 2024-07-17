import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8

import Control.Concurrent
import Options.Applicative
import System.FilePath

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2


-- data Entry = Entry { langName :: String, perf3 :: Int, totalChars :: Int} deriving Show

data Options = Options {
  connectionInfo :: Connection,
  keys           :: KeyFiles,
  script         :: FilePath,
  logFile        :: FilePath
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

options :: Parser Options
options = Options <$> connectionParser <*> keyFilesParser <*> scriptParser <*> logFileParser

runCommand :: Session -> String -> IO (Int, BSL.ByteString)
runCommand s cmd = withChannel s $ \ch -> do
  channelExecute ch cmd
  readAllChannel ch

parseSubmissionResult :: (Int, BSL.ByteString) -> String
parseSubmissionResult = BSL8.unpack . head . BSL8.split '.' . snd

parseQstatResponse :: (Int, BSL.ByteString) -> String
parseQstatResponse r = head $ tail $ reverse $ words $ (lines (BSL8.unpack $ snd r) !! 2)

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

  -- Authenticate (Leave knownHosts filepath as empty string to prevent strict checking)
  publicKeyAuthFile session (user $ connectionInfo opts) (publicKey $ keys opts) (privateKey $ keys opts) ""
  putStrLn "Authorised"

  -- Send a file to remote host via SCP.
  scriptSize <- scpSendFile session 0o644 (script opts) (takeFileName $ script opts)
  putStrLn $ "Sent: " ++ script opts ++ " - "++ show scriptSize ++ " bytes."

  -- Submit job using script file
  submissionResult <- runCommand session ("qsub " ++ takeFileName (script opts))

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
