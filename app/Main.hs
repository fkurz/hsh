module Main where

import Control.Monad (unless, when, return, join, (>=>))
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import System.IO (BufferMode(..), isEOF, hSetBuffering, stdout, hFlush)
import System.Directory (getCurrentDirectory)
import Prelude (IO, Int, String, Either(..), Maybe(..), Show, not, show, getLine, map, putStr, putStrLn, show, unwords, ($), (.), (++), (>>), (>>=), (<$>))
import Language.Haskell.Interpreter (Interpreter, InterpreterError(..), GhcError(..), Extension(OverloadedStrings), OptionVal((:=)), loadModules, setImports, interpret, as, runInterpreter, set, languageExtensions)
import System.FilePath.Posix (joinPath)

import Lib (CommandLineExecutionResult)

data Configuration = Configuration {
  moduleListOf :: [String],
  importListOf :: [String],
  languageExtensionsOf :: [Extension]
}

main = do 
  configuration <- buildConfiguration
  processCommands configuration

buildConfiguration :: IO Configuration
buildConfiguration = do 
  currentWorkingDirectory <- getCurrentDirectory
  let libModulePath = joinPath [currentWorkingDirectory, "src/Lib.hs"]
  return $ Configuration { 
    moduleListOf = [libModulePath] 
    , importListOf = ["Prelude", "Lib", "Text.Format"] 
    , languageExtensionsOf = [OverloadedStrings]
  }

processCommands :: Configuration -> IO ()
processCommands configuration = do
  commandLine' <- prompt drawPrompt
  case commandLine' of 
    Just commandLine -> do 
      processCommandLine configuration commandLine errorHandler successHandler
      processCommands configuration
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

processCommandLine :: Configuration -> String -> (String -> IO ()) -> (String -> IO ()) -> IO ()
processCommandLine configuration commandLine errorHandler successHandler = do
  commandLineInterpreterResult <- runCommandLineInterpreter configuration commandLine
  processCommandLineInterpreterResult commandLineInterpreterResult errorHandler successHandler

runCommandLineInterpreter configuration commandLine = runInterpreter $ do 
    loadModules $ moduleListOf configuration
    setImports $ importListOf configuration
    set [languageExtensions := languageExtensionsOf configuration]
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

