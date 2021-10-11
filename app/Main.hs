module Main where

import Control.Monad (unless, when, return, join)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import System.IO (BufferMode(..), isEOF, hSetBuffering, stdout, hFlush)
import Prelude (IO, Int, String, Either(..), Maybe(..), Show, not, show, getLine, map, putStr, putStrLn, show, unwords, ($), (.), (++), (>>), (>>=), (<$>))
import Language.Haskell.Interpreter (Interpreter, InterpreterError(..), GhcError(..), loadModules, setImports, interpret, as, runInterpreter)
import Lib (CommandLineExecutionResult)

{-
TODOs
- parse and interpret code (/)
- implement echo + cd + cwd + calc
  - echo (/)
  - cd ()
  - cwd (/)
  - calc (/)
  - print/printf ()
  - ls ()
- implement pipe operator 
  - cwd & ls
  - calc $ 1 + 1 & echo
- input mode/direct execution mode (ESC -> input mode -> reads input and aggregates until ESC pressed again; otherwise direct execution)
- transactional behavior/execution plans
- multiline input 

  $ hsh 1 + \
  ...  2 + \
  ...  3
  6
- syntax highlighting
- autocompletion
- draw prompt
  - using starship toml format?
-}

main = processCommands

processCommands :: IO ()
processCommands = do
  commandLine' <- prompt drawPrompt
  case commandLine' of 
    Just commandLine -> do 
      processCommandLine commandLine errorHandler successHandler
      processCommands
    Nothing -> do 
      putStrLn "Exiting hsh"

-- See https://stackoverflow.com/a/13190872
prompt :: String -> IO (Maybe String)
prompt drawPrompt = do
    putStr drawPrompt
    hFlush stdout
    hasMoreInput <- not <$> isEOF
    if hasMoreInput 
      then Just <$> getLine
      else return Nothing

drawPrompt = "\n> "

type CommandLineInterpreterResult = Either InterpreterError (IO CommandLineExecutionResult)

processCommandLine :: String -> (String -> IO ()) -> (String -> IO ()) -> IO ()
processCommandLine commandLine errorHandler successHandler = do
  commandLineInterpreterResult <- runCommandLineInterpreter commandLine
  processCommandLineInterpreterResult commandLineInterpreterResult errorHandler successHandler

runCommandLineInterpreter commandLine = runInterpreter $ do 
    loadModules ["src/Lib.hs"]
    setImports ["Prelude", "Lib"]
    interpret commandLine (as :: IO CommandLineExecutionResult)

processCommandLineInterpreterResult :: CommandLineInterpreterResult  -> (String -> IO ()) -> (String -> IO ()) -> IO ()
processCommandLineInterpreterResult commandLineInterpreterResult errorHandler successHandler = case commandLineInterpreterResult of 
  Left interpreterError -> errorHandler $ showInterpreterError interpreterError
  Right interpreterResult' -> do 
    interpreterResult <- interpreterResult'
    case interpreterResult of 
      Left error -> errorHandler error
      Right success -> successHandler success

showInterpreterError :: InterpreterError -> String
showInterpreterError (WontCompile es) = intercalate "\n" $ map unbox es
  where unbox (GhcError e) = e
showInterpreterError e = show e

errorHandler :: String -> IO ()
errorHandler = putStr . (++) "e: " 

successHandler :: String -> IO ()
successHandler [] = putStr ""
successHandler output = putStr $ (++) "λ: " output

