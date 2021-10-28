-- Required by argument collection
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
-- Required by Text.Format
{-# LANGUAGE OverloadedStrings #-}
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

-- | TODO FK probably not the best naming because it clashes with print in Prelude. Consider PrettyPrintable?
-- See https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:print
-- | Printable is only applicable to types with a Show instance. 
-- This way, we ensure that we can provide some kind of human readable output (even if it may not be pretty printed).
class Show a => Printable a where
    print :: a -> IO ()
    print = putStrLn . show 

instance Printable () where 
    print _ = putStr ""

instance Printable Int

instance Printable String where
    print = putStrLn

instance Printable [String] where   
    print (x : xs) = print x >> print xs
    print _ = putStr ""

cd :: FilePath -> IO ()
cd filePath = do
    setCurrentDirectory filePath

cwd :: IO String
cwd = getCurrentDirectory

echo :: Show a => a -> IO a
echo = return 

ls :: FilePath -> IO [FilePath]
ls = listDirectory