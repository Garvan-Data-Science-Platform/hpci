module Helpers (
  sessionInit'
  , publicKeyAuthFile'
  , scpSendFile'
  , scpReceiveFile'
  , runCommand
  , parseExecResult
) where

import qualified Data.Text as T
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

sessionInit' :: Host -> Port -> IO Session
sessionInit' (Host host) (Port port) = sessionInit (T.unpack host) port

-- Default to not using a passphrase
publicKeyAuthFile' :: Session -> User -> PublicKey -> PrivateKey -> IO ()
publicKeyAuthFile' s (User user) (PublicKey publicKey) (PrivateKey privateKey) = publicKeyAuthFile s (T.unpack user) publicKey privateKey ""

-- TODO: Make script location configurable
-- TODO: Make file creation mode configurable
scpSendFile' :: Session -> Script -> IO Integer
scpSendFile' s (Script script) = scpSendFile s 0o644 script (takeFileName script)

-- scpReceiveFile wrap_up_session (logFile $ optCommand opts) (takeFileName $ logFile $ optCommand opts)
scpReceiveFile' :: Session -> LogFile -> IO Integer
scpReceiveFile' s (LogFile logFile) = scpReceiveFile s logFile (takeFileName logFile)
