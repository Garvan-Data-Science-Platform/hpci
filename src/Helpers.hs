module Helpers (
  safeSessionInit
  , safePublicKeyAuthFile
  , safeScpSendFile
  , safeScpReceiveFile
  , runCommand
  , parseExecResult
) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8

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
  , LogFile(..))


runCommand :: Session -> String -> IO (Int, BSL.ByteString)
runCommand s cmd = withChannel s $ \ch -> do
  channelExecute ch cmd
  readAllChannel ch

parseExecResult :: (Int, BSL.ByteString) -> (Int, String)
parseExecResult (i, bs) = (i, BSL8.unpack bs)

safeSessionInit :: Host -> Port -> IO Session
safeSessionInit (Host host) (Port port) = sessionInit host port

-- Default to not using a passphrase
safePublicKeyAuthFile :: Session -> User -> PublicKey -> PrivateKey -> IO ()
safePublicKeyAuthFile s (User user) (PublicKey publicKey) (PrivateKey privateKey) = publicKeyAuthFile s user publicKey privateKey ""

-- TODO: Make script location configurable
-- TODO: Make file creation mode configurable
safeScpSendFile :: Session -> Script -> IO Integer
safeScpSendFile s (Script script) = scpSendFile s 0o644 script (takeFileName script)

-- scpReceiveFile wrap_up_session (logFile $ optCommand opts) (takeFileName $ logFile $ optCommand opts)
safeScpReceiveFile :: Session -> LogFile -> IO Integer
safeScpReceiveFile s (LogFile logFile) = scpReceiveFile s logFile (takeFileName logFile)
