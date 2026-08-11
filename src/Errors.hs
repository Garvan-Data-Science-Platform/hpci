module Errors (
  wrapSshErrors
  , renderSshError
  , HpciUserError(..)
) where

import Control.Exception (
  Exception(..)
  , Handler(..)
  , catches
  , throwIO
  , IOException)
import Data.List (isInfixOf)
import Data.Typeable (Typeable)
import Network.SSH.Client.LibSSH2.Errors (
  ErrorCode(..)
  , SftpErrorCode(..))
import System.IO.Error (isDoesNotExistError, ioeGetLocation)

data HpciUserError
  = SshError String
  | NetworkError String
  | LocalFileError String
  deriving (Typeable)

instance Show HpciUserError where
  show (SshError message) = "hcpi SSH Error: " ++ message
  show (NetworkError message) = "hcpi Network Error: " ++ message
  show (LocalFileError message) = "hcpi Local File Error: " ++ message

instance Exception HpciUserError where
  displayException (SshError message) = "SSH Connection Failed\n" ++ message
  displayException (NetworkError message) = "Network Connection Failed\n" ++ message
  displayException (LocalFileError message) = "Local File Missing Or Inaccessible\n" ++ message

renderSshError :: ErrorCode -> String
renderSshError err = case err of
  FILE ->
    "Could not read your SSH keys.\n" ++
    "Fix: Ensure keys exist and have the correct permissions."

  AUTHENTICATION_FAILED ->
    "The HPC rejected your login attempt.\n" ++
    "Fix: ensure your username is correct and that your public ssh key is in the login node's authorized_keys file."

  TIMEOUT ->
    "The connection to the HPC timed out.\n" ++
    "Fix: Check the cluster network connection."

  -- Not currently possible, but leaving in regardless
  ERROR_KNOWN_HOSTS ->
    "The HPC login node's identity does not match your known_hosts file.\n" ++
    "Fix: Remove the old key from your runner's known_hosts file"

  KEX_FALIURE ->
   "Encryption key exchange failed. The HPC node is using incompantible SSH settings."

  SCP_PROTOCOL ->
    "An SCP file transfer failed. The remote file does not exist or access was denied.\n" ++
    "Fix: ensure HPC job log file was created at the specified path"

  _ -> "An unexpected SSH error occured: " ++ show err ++ ")."

renderSftpError :: SftpErrorCode -> String
renderSftpError err = case err of
  FX_NO_SUCH_FILE -> "The requested remote file or directory does not exist on the HPC node."
  FX_PERMISSION_DENIED -> "You do not have permission to access this remote file."
  _ -> "An unexpected SFTP error occured (Code: " ++ show err ++ ")."

wrapSshErrors :: IO a -> IO a
wrapSshErrors action = action `catches` [Handler handleSsh, Handler handleSftp, Handler handleIO]
  where
    handleSsh :: ErrorCode -> IO a
    handleSsh err = throwIO $ SshError $ renderSshError err

    handleSftp :: SftpErrorCode -> IO a
    handleSftp err = throwIO $ SshError $ renderSftpError err

    handleIO :: IOException -> IO a
    handleIO err
      | isDoesNotExistError err && not ("Network.Socket.connect" `isInfixOf` displayException err) = throwIO $ LocalFileError $
      "Could not read a required local file before sending it to the HPC.\n" ++
      "System Error: " ++ displayException err ++ "\n" ++
      "Fix: ensure the file exists at the path provided by your CLI arguments"

      | otherwise = throwIO $ NetworkError $
      "Could not establish a network connection to the HPC node.\n" ++
      "System Error: " ++ displayException err ++ "\n" ++
      "Fix: Check that host IP and port are correct."
