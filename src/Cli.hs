module Cli (
  Options(..),
  Command(..),
  Connection(..),
  parseOptions,
  parsePair
  ) where


import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Options.Applicative

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
    let pairs = T.splitOn (T.pack ",") (T.pack input)
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

-- Top-level parser for options
options :: Parser Options
options = Options <$> connectionParser <*> commandParser

-- Function to run the parser
parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
      <> progDesc "Integrate HPC with CI"
      <> header "Schedule jobs and execute commands on HPC via CI" )
