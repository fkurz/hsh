-- Required by argument collection
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
-- Required by Text.Format
{-# LANGUAGE OverloadedStrings #-}
-- TODO
{-# LANGUAGE DefaultSignatures #-}

module Lib where

import Prelude (IO, Int, Integral, Either(..), String, Show, Maybe(..), Char, id, map, zip, concat, show, reverse, putStrLn, putStr, putChar, return, toEnum, ($), (.), (<$>), (++), (>>), (>>=))
import System.Directory (getCurrentDirectory, setCurrentDirectory, listDirectory)
import System.IO (FilePath)
import Control.Monad.IO.Class (liftIO)
import Text.Format (FormatType, Format, Format1, Formatter, FormatArg, ArgKey(..), FromArgKey(..), format, format1, formatString, sfmt, defaultOptions, genericFormatArg)
import Data.String (IsString)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Bifunctor (second)

cd :: FilePath -> IO ()
cd filePath = setCurrentDirectory filePath

cwd :: IO String
cwd = getCurrentDirectory

echo :: Show a => a -> IO a
echo = return 

ls :: FilePath -> IO [FilePath]
ls = listDirectory