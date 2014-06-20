import System.Environment

import PTEstruturado.Interpreter

main = do
  args     <- getArgs
  progName <- getProgName
  state <- if null args
              then error $ "usage: "++progName++" file_to_run"
              else runfile $ head args
  putStrLn "----------------------------------------------------"
  mapM_ print $ snd state 
