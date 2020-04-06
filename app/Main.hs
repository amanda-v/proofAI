module Main (main) where

import qualified System.IO as IO
import Run.Options (parseOptions)
import Run.Orchestrator (run)

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  parseOptions >>= run