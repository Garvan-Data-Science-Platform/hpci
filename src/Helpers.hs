{-# LANGUAGE RankNTypes #-}

module Helpers (
  scpSendFileRetry
  , scpReceiveFileRetry
  , runCommand
  , parseExecResult
  , connectWithRetry
  , defaultRetryPolicy
  , sessionRetry
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Control.Retry (
  exponentialBackoff
  , limitRetries
  , recoverAll
  , RetryPolicy)

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
sessionInit' (Host h) (Port p) = sessionInit (T.unpack h) p

-- Note: Default to not using a passphrase
publicKeyAuthFile' :: Session -> User -> PublicKey -> PrivateKey -> IO ()
publicKeyAuthFile' s (User u) (PublicKey publicK) (PrivateKey privateK) = publicKeyAuthFile s (T.unpack u) publicK privateK ""

-- TODO: Make script location configurable
-- TODO: Make file creation mode configurable
scpSendFileRetry :: RetryPolicy -> Session -> Script -> IO Integer
scpSendFileRetry policy s (Script script) =
  sessionRetry policy (scpSendFile s 0o644 script (takeFileName script))

scpReceiveFileRetry :: RetryPolicy -> Session -> LogFile -> IO Integer
scpReceiveFileRetry policy s (LogFile logFile) =
  sessionRetry policy (scpReceiveFile s logFile (takeFileName logFile))

-- | helpers to help with Retries
defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = exponentialBackoff 1000000 <> limitRetries 5

-- Note: I make this polymorphic to work with both IO Session
--   and IO MockSession in tests
sessionRetry :: RetryPolicy -> IO a -> IO a
sessionRetry policy actionToRetry =
  recoverAll policy (\_ -> actionToRetry)

connect :: Connection -> IO Session
connect connInfo = do
  s <- sessionInit' (host connInfo) (port connInfo)
  publicKeyAuthFile' s (user connInfo) (publicKey connInfo) (privateKey connInfo)
  putStrLn "SSH session established"
  return s

-- add policy arg
connectWithRetry :: Connection -> IO Session
connectWithRetry connInfo =
  sessionRetry defaultRetryPolicy (connect connInfo)

