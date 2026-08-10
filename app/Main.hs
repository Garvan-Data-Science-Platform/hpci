module Main where

import Control.Exception (catch, displayException)
import System.Exit (exitFailure)

import Cli
import Exec
import Schedule
import Errors (wrapSshErrors, HpciUserError)

runHpci :: Options -> IO ()
runHpci opts = wrapSshErrors $ do
  case optCommand opts of 
    Exec execStr -> runExec opts execStr

    _            -> runSchedule opts

handleHpciError :: HpciUserError -> IO ()
handleHpciError e = do
  putStrLn $ displayException e
  exitFailure

main :: IO ()
main = do
  opts <- parseOptions
  runHpci opts `catch` handleHpciError
