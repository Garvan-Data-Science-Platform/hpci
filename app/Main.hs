import Cli
import Exec
import Schedule

runHpci :: Options -> IO ()
runHpci opts = do
  case optCommand opts of 
    Exec execStr -> runExec opts execStr

    _    -> runSchedule opts

main :: IO()
main = do
  opts <- parseOptions
  runHpci opts
