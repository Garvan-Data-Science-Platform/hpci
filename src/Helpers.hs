module Helpers (
  scpSendFile'
  , scpReceiveFile'
  , runCommand
  , parseExecResult
  , connectWithRetry
  , defaultRetryPolicy
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Control.Retry (
  exponentialBackoff
  , limitRetries
  , recoverAll
  , RetryPolicyM)

import Network.SSH.Client.LibSSH2.Foreign (
  publicKeyAuthFile
  , channelExecute)
import Network.SSH.Client.LibSSH2 (
  Session
  , sessionInit
  , scpSendFile
  , scpReceiveFile
  , readAllChannel
  , withChannel)
import System.FilePath

import Cli (
    User(..)
  , Host(..)
  , Port(..)
  , PublicKey(..)
  , PrivateKey(..)
  , Script(..)
  , LogFile(..)
  , Connection(..)
  )

runCommand :: Session -> String -> IO (Int, BSL.ByteString)
runCommand s cmd = withChannel s $ \ch -> do
  channelExecute ch cmd
  readAllChannel ch

parseExecResult :: (Int, BSL.ByteString) -> (Int, String)
parseExecResult (i, bs) = (i, BSL8.unpack bs)

-- | Wrap ssh library to use text
sessionInit' :: Host -> Port -> IO Session
sessionInit' (Host host) (Port port) = sessionInit (T.unpack host) port

-- Note: Default to not using a passphrase
publicKeyAuthFile' :: Session -> User -> PublicKey -> PrivateKey -> IO ()
publicKeyAuthFile' s (User user) (PublicKey publicKey) (PrivateKey privateKey) = publicKeyAuthFile s (T.unpack user) publicKey privateKey ""

-- TODO: Make script location configurable
-- TODO: Make file creation mode configurable
scpSendFile' :: Session -> Script -> IO Integer
scpSendFile' s (Script script) = scpSendFile s 0o644 script (takeFileName script)

scpReceiveFile' :: Session -> LogFile -> IO Integer
scpReceiveFile' s (LogFile logFile) = scpReceiveFile s logFile (takeFileName logFile)

-- | helpers to help with Retries
defaultRetryPolicy :: (RetryPolicyM IO)
defaultRetryPolicy = exponentialBackoff 1000000 <> limitRetries 5

sessionRetry :: RetryPolicyM IO -> IO Session -> IO Session
sessionRetry policy actionToRetry =
  recoverAll policy (\_ -> actionToRetry)

connect :: Connection -> IO Session
connect connInfo = do
  s <- sessionInit' (host connInfo) (port connInfo)
  publicKeyAuthFile' s (user connInfo) (publicKey connInfo) (privateKey connInfo)
  putStrLn "SSH session established"
  return s

connectWithRetry :: Connection -> IO Session
connectWithRetry connInfo =
  sessionRetry defaultRetryPolicy (connect connInfo)

