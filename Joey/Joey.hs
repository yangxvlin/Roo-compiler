module Main (main) 
where
import JoeyParser (ast)
import PrettyJoey (pp)
import System.Environment (getProgName, getArgs)
import System.Exit (exitWith, ExitCode(..))

data Task
  = Parse | Pprint 
    deriving (Eq, Show)

main :: IO ()
main
  = do
      progname <- getProgName
      args <- getArgs
      task <- checkArgs progname args
      case task of
        Parse
          -> do
               let [_, filename] = args
               input <- readFile filename
               let output = ast input
               case output of
                 Right tree 
                   -> putStrLn (show tree)
                 Left err 
                   -> do putStrLn "Parse error at " 
                         print err
                         exitWith (ExitFailure 2) 
        Pprint
          -> do
               let [_, filename] = args
               input <- readFile filename
               let output = ast input
               case output of
                 Right tree 
                   -> putStrLn (pp tree)
                 Left err 
                   -> do putStrLn "Parse error at "
                         print err
                         exitWith (ExitFailure 2) 

checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_]
  = do
      putStrLn ("Missing filename")
      exitWith (ExitFailure 1)
checkArgs _ ["-a", filename]
  = return Parse
checkArgs _ ["-p", filename]
  = return Pprint
checkArgs progname _
  = do
      putStrLn ("Usage: " ++ progname ++ " [-p] filename")
      exitWith (ExitFailure 1)

