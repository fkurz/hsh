{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Control.Monad (unless, when, return, join, (>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadMask)
import Data.List (intercalate)
import System.IO (BufferMode(..), isEOF, hSetBuffering, stdout, hFlush)
import System.Directory (getCurrentDirectory)
import Prelude (IO, Int, String, Either(..), Maybe(..), Show, id, not, show, getLine, map, putStr, putStrLn, show, unwords, ($), (.), (++), (>>), (>>=), (<$>)) 
import Language.Haskell.Interpreter (Interpreter, InterpreterError(..), GhcError(..), Extension(OverloadedStrings), OptionVal((:=)), loadModules, setImports, interpret, as, runInterpreter, set, languageExtensions)
import System.FilePath.Posix (joinPath)

import Lib (Printable(..))

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
      processCommandLine configuration commandLine
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

drawPrompt = "λ> "

processCommandLine :: Configuration -> String -> IO ()
processCommandLine configuration commandLineExpression = do
  commandLineInterpreterResult <- runCommandLineInterpreter configuration commandLineExpression
  case commandLineInterpreterResult of 
    Left interpreterError -> putStrLn $ showInterpreterError interpreterError
    Right interpreterSuccess -> interpreterSuccess 

-- | To make our lives easier, we require that all expressions passed via the command line are of type @IO a@ where @a@ has a @Printable@ instance.
-- This allows us to make the interpretation result monomorphic and moreover enforce that shell commands provide some kind of human readable output.
runCommandLineInterpreter :: (MonadIO m, MonadMask m) => Configuration -> String -> m (Either InterpreterError (IO ())) 
runCommandLineInterpreter configuration commandLineExpression = runInterpreter $ do 
    loadModules $ moduleListOf configuration
    setImports $ importListOf configuration
    set [languageExtensions := languageExtensionsOf configuration]
    let printedCommandLineExpression = "(" ++ commandLineExpression ++ ") >>= Lib.print"
    interpret printedCommandLineExpression (as :: IO ())

showInterpreterError :: InterpreterError -> String
showInterpreterError (WontCompile es) = intercalate "\n" $ map unbox es
  where unbox (GhcError e) = e
showInterpreterError e = show e
