import System.Console.GetOpt
import System.Environment(getArgs, getProgName)

import PTEstruturado.Interpreter
import PTEstruturado.Parse


data Flag
    = Source String
    | JustParse
    | ShowMemory
    deriving Show

options :: [OptDescr Flag]
options = 
    [ Option ['P'] ["just-parse" ] (NoArg JustParse     ) "no exec, Just parse and print result"
    , Option ['M'] ["show-memory"] (NoArg ShowMemory    ) "print variables in the end of execution"
    ]
    
compilerOpts :: String -> [String] -> IO ([Flag], [String])
compilerOpts name argv = 
   case getOpt Permute options argv of
        (o,n,[]  )
          | not (null n) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: "++name++" [OPTION] file"    
  
main = do
  args <- getArgs
  name <- getProgName
  opts <- compilerOpts name args
  case opts of 
       ([]           , file:_) -> runfile file >> return ()
       ([JustParse]  , file:_) -> parseFile file >>= print 
       ([ShowMemory] , file:_) -> do
         (_, state) <- runfile file
         putStrLn "----------------------------------------------------"
         mapM_ print $ state 
