module Run.Orchestrator (run) where


import Run.Options
import qualified Run.Orchestrator.Interactive as Interactive
import qualified Run.Orchestrator.Noninteractive as Noninteractive
import Run.Player


run :: Options -> IO ()
run (Options Computer Computer first rounds) =
  Noninteractive.run first rounds

run (Options x o first _) =
  Interactive.run x o first
