-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yang                            --
-----------------------------------------------------------
module Main (main) 
where
import RooParser (ast)
import PrettyRoo (pp)
import RooAnalyser (analyse, Result(..))
import OzCode (writeCode)
import Codegen (ozCode)
import System.Environment (getProgName, getArgs)
import System.Exit (exitWith, ExitCode(..))

data Task
  = Parse | Pprint | NoOz | Compile 
    deriving (Eq, Show)

main :: IO ()
main
  = do
      progname <- getProgName
      args <- getArgs
      task <- checkArgs progname args
      case task of
        Compile
          -> do
               input <- readFile (head args)
               let output = ast input
               case output of
                 Right tree
                   -> do let pt = analyse tree
                         case pt of
                           Err err
                             -> do putStrLn err
                                   exitWith (ExitFailure 2)
                           Okay table
                             -> do let code = ozCode table tree
                                   putStrLn (writeCode code)
                 Left err
                   -> do putStr "Parse error at "
                         print err
                         exitWith (ExitFailure 2)
               exitWith ExitSuccess
        NoOz
          -> do
               let [_, filename] = args
               input <- readFile filename
               let output = ast input
               case output of
                 Right tree
                   -> do let pt = analyse tree
                         case pt of
                           Err err
                             -> do putStrLn err
                                   exitWith (ExitFailure 2)
                           Okay _
                             -> putStrLn "Roo program appears well-formed"
                 Left err
                   -> do putStr "Parse error at "
                         print err
                         exitWith (ExitFailure 2)
               exitWith ExitSuccess
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
                   -> putStr (pp tree)
                 Left err 
                   -> do putStrLn "Parse error at "
                         print err
                         exitWith (ExitFailure 2) 

checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_]
  = do
      putStrLn ("Missing filename")
      exitWith (ExitFailure 1)
checkArgs _ [filename]
  = return Compile
checkArgs _ ["-a", filename]
  = return Parse
checkArgs _ ["-p", filename]
  = return Pprint
checkArgs _ ["-n", filename]
  = return NoOz
checkArgs progname _
  = do
      putStrLn ("Usage: " ++ progname ++ " [-p] filename")
      exitWith (ExitFailure 1)

