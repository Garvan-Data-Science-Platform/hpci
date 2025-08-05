{-# LANGUAGE OverloadedStrings #-}

module Cli (
  Options(..)
  , Command(..)
  , Connection(..)
  , User(..)
  , Host(..)
  , Port(..)
  , PublicKey(..)
  , PrivateKey(..)
  , Script(..)
  , LogFile(..)
  , parseOptions
  , parsePair
  ) where


import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Options.Applicative

-- Types to avoid using primitives
newtype User = User Text deriving (Show)
newtype Host = Host Text deriving (Show)
newtype Port = Port Int deriving (Show)
newtype PublicKey = PublicKey FilePath deriving (Show)
newtype PrivateKey = PrivateKey FilePath deriving (Show)
newtype Script = Script FilePath deriving (Show)
newtype LogFile = LogFile FilePath deriving (Show)

-- Data types for CLI options

data Options = Options {
  connectionInfo :: Connection,
  optCommand     :: Command
} deriving (Show)

data Command
  = Schedule {
      script         :: Script,
      logFile        :: LogFile,
      optConfig      :: KeyValuePairs }
  | Exec Text deriving Show

data Connection = Connection {
  user       :: User,
  host       :: Host,
  port       :: Port,
  publicKey  :: PublicKey,
  privateKey :: PrivateKey
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
    userParser = User <$> strOption (long "user" <> help "Username")
    hostParser = Host <$> strOption (long "host" <> help "Hostname")
    portParser = Port <$> option auto (long "port" <> help "Port number" <> metavar "INT")
    publicKeyParser = PublicKey <$> strOption (long "publicKey" <> help "Public ssh key file path")
    privateKeyParser = PrivateKey <$> strOption (long "privateKey" <> help "Private ssh key file path")

scriptParser :: Parser Script
scriptParser = Script <$> strOption (long "script")

logFileParser :: Parser LogFile
logFileParser = LogFile <$> strOption (long "logFile")

type KeyValuePairs = Map Text Text

-- Wrapper to handle string to text
parseKeyValuePairsReader' :: String -> Either String KeyValuePairs
parseKeyValuePairsReader' = first T.unpack . parseKeyValuePairs . T.pack

keyValuePairsReader :: ReadM KeyValuePairs
keyValuePairsReader = eitherReader parseKeyValuePairsReader'

-- Function to parse a series of comma separated key-value pairs
parseKeyValuePairs :: Text -> Either Text KeyValuePairs
parseKeyValuePairs input =
    let pairs = T.splitOn "," input
        parsedPairs = mapM (parsePair "=") pairs
    in fmap Map.fromList parsedPairs

-- Function to parse key value pairs from text where
  -- keys and values are separated by `sep`
parsePair :: Text -> Text -> Either Text (Text, Text)
parsePair sep pair = case T.splitOn sep pair of
    [k, v] -> Right (k, v)
    _      -> Left ("Invalid key-value pair: " <> pair)

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
