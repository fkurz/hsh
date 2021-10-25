-- Required by argument collection
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
-- Required by Text.Format
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Prelude (IO, Int, Integral, Either(..), String, Show, Maybe(..), map, zip, concat, show, reverse, putStrLn, putStr, return, toEnum, ($), (.), (<$>), (++))
import System.Directory (getCurrentDirectory, setCurrentDirectory, listDirectory)
import System.IO (FilePath)
import Control.Monad.IO.Class (liftIO)
import Text.Format (FormatType, Format, Format1, Formatter, FormatArg, ArgKey(..), FromArgKey(..), format, format1, formatString, sfmt, defaultOptions, genericFormatArg)
import Data.String (IsString)
import qualified Data.Map as Map
import Data.Bifunctor (second)

type CommandLineExecutionResult = Either String String

calc :: Int -> IO CommandLineExecutionResult
calc integer = liftIO $ Right <$> liftIO (return $ show integer)

cd :: FilePath -> IO CommandLineExecutionResult
cd filePath = do
    setCurrentDirectory filePath
    return $ Right ""

cwd :: IO CommandLineExecutionResult
cwd = liftIO $ Right <$> getCurrentDirectory

echo :: String -> IO CommandLineExecutionResult
echo message = do
    putStrLn message
    return $ Right ""

ls :: FilePath -> IO CommandLineExecutionResult
ls filePath = Right . show <$> listDirectory filePath

print :: String -> IO CommandLineExecutionResult
print message = do
    putStr message
    return $ Right ""

instance FromArgKey Int where 
    fromArgKey (Index integer) = Just $ toEnum integer
    fromArgKey _ = Nothing

printf :: (IsString a, Show a) => Format -> [a] -> IO CommandLineExecutionResult
printf formattingPattern formattingItems = return . Right $ sfmt formattingPattern formatterArguments
    where
        formatterArguments :: Map.Map ArgKey Formatter
        formatterArguments = Map.fromList $ map (\(index, formattingItem) -> (Index index, formatString $ show formattingItem)) enumeratedFormattingItems
        enumeratedFormattingItems = zip ([0..] :: [Int]) formattingItems