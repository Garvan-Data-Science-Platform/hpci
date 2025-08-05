module Main (
  main
) where

import Test.Tasty
import Test.Tasty.Program

import Paths_hpci (getBinDir)
import System.FilePath ((</>))

tests :: FilePath -> TestTree
tests executablePath =
  testGroup "HPCI Integration Tests"
    [
      testProgram
        "Test with dockerised OpenPBS"
        "make"
        [ "HPCI_EXE=" <> executablePath
        , "test-schedule"
        ]
        Nothing
    ]

main :: IO ()
main = do
  binDir <- getBinDir
  let exePath = binDir </> "hpci-exe"

  defaultMain (tests exePath)
