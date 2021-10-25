{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

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

drawPrompt = "\nλ> "

type CommandLineInterpreterResult = Either InterpreterError (IO CommandLineExecutionResult)

processCommandLine :: Configuration -> String -> IO ()
processCommandLine configuration commandLine = do
  commandLineInterpreterResult <- runCommandLineInterpreter configuration commandLine
  print commandLineInterpreterResult

runCommandLineInterpreter configuration commandLine = runInterpreter $ do 
    loadModules $ moduleListOf configuration
    setImports $ importListOf configuration
    set [languageExtensions := languageExtensionsOf configuration]
    interpret commandLine (as :: IO CommandLineExecutionResult)

class Show a => Printable a where
  print :: a -> IO ()
  print x = putStr $ show x

instance Printable String where 
  print = putStr 

instance Printable CommandLineExecutionResult where
  print (Right result) = print $ "success: " ++ result
  print (Left error) = print $ "error: " ++ error

-- | TODO FK an ugly hack to allow the Printable instance for CommandLineInterpreterResult since
-- its result type IO CommandLineExecutionResult does not have—rightly so!—a Show instance.
instance Show (IO CommandLineExecutionResult) where
  show _ = ""

instance Printable CommandLineInterpreterResult where
  print (Right result) = result >>= print
  print (Left error) = putStr $ showInterpreterError error

showInterpreterError :: InterpreterError -> String
showInterpreterError (WontCompile es) = intercalate "\n" $ map unbox es
  where unbox (GhcError e) = e
showInterpreterError e = show e
