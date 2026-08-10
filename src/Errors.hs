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
import Data.Typeable (Typeable)
import Network.SSH.Client.LibSSH2.Errors (
  ErrorCode(..)
  , SftpErrorCode(..))

newtype HpciUserError = HpciUserError String
  deriving (Typeable)

instance Show HpciUserError where
  show (HpciUserError message) = "hcpi error: " ++ message

instance Exception HpciUserError where
  displayException (HpciUserError message) = "SSH Connection Failed\n" ++ message
  -- TODO why separate errors for ssh connection failures?

renderSshError :: ErrorCode -> String
renderSshError err = case err of
  FILE ->
    "Could not read your SSH keys or known_hosts file.\n" ++ -- TODO check if known_hosts error is possible with my code?
    "Fix: Ensure keys exist and have the correct permissions."

  AUTHENTICATION_FAILED ->
    "The HPC rejected your login attempt.\n" ++
    "Fix: ensure your public key is in the login node's authorized_keys file."

  TIMEOUT ->
    "The connection to the HPC timed out.\n" ++
    "Fix: Check the cluster network connection."

  ERROR_KNOWN_HOSTS ->
    "The HPC login node's identity does not match your known_hosts file.\n" ++ -- TODO check if we're using known_hosts
    "Fix: Remove the old key from your runner's known_hosts file"

  KEX_FALIURE ->
   "Encryption key exchange failed. The HPC node is using incompantible SSH settings."

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
    handleSsh err = throwIO $ HpciUserError $ renderSshError err

    handleSftp :: SftpErrorCode -> IO a
    handleSftp err = throwIO $ HpciUserError $ renderSftpError err

    handleIO :: IOException -> IO a
    handleIO err = throwIO $ HpciUserError $
      "Could not establish a network connection to the HPC node.\n" ++
      "System Error: " ++ displayException err ++ "\n" ++
      "Fix: Check that host IP and port are correct."
