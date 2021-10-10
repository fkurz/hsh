module Main where

import Control.Monad (unless, when, return, join)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import System.IO (BufferMode(..), isEOF, hSetBuffering, stdout, hFlush)
import Prelude (IO, Int, String, Either(..), Maybe(..), Show, not, show, getLine, map, putStr, putStrLn, show, unwords, ($), (.), (++), (>>), (>>=), (<$>))
import Language.Haskell.Interpreter (Interpreter, InterpreterError(..), GhcError(..), loadModules, setImports, interpret, as, runInterpreter)
import Lib (CommandLineInterpreterResult)

{-
TODOs
- parse and interpret code (/)
- implement echo + cd + cwd + calc
  - echo (/)
  - cd ()
  - cwd (/)
  - calc (/)
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

-- TODO FK rm?
-- say :: String -> Interpreter ()
-- say = liftIO . putStrLn

main = processCommands

processCommands :: IO ()
processCommands = do
  commandLine' <- prompt drawPrompt
  case commandLine' of 
    Just commandLine -> do 
      interpretCommandLine commandLine errorHandler successHandler
      processCommands
    Nothing -> do 
      putStrLn "Exiting hsh"

-- See https://stackoverflow.com/a/13190872
prompt :: String -> IO (Maybe String)
prompt promptText = do
    putStr promptText
    hFlush stdout
    hasMoreInput <- not <$> isEOF
    if hasMoreInput 
      then Just <$> getLine
      else return Nothing

drawPrompt = "\n> "

-- interpretCommandLine :: String -> Interpreter ()
interpretCommandLine :: String -> (String -> IO()) -> (String -> IO()) -> IO ()
interpretCommandLine commandLine errorHandler successHandler = do
  interpretedCommandLine <- runInterpreter $ do 
    loadModules ["src/Lib.hs"]
    setImports ["Prelude", "Lib"]
    interpret commandLine (as :: IO CommandLineInterpreterResult)
  case interpretedCommandLine of 
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

