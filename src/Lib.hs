module Lib where 

import Prelude (IO, Int, Either(..), String, putStrLn, return, show, ($), (.), (<$>))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.IO (FilePath)
import Control.Monad.IO.Class (liftIO)

type CommandLineInterpreterResult = Either String String

calc :: Int -> IO CommandLineInterpreterResult
calc integer = liftIO $ Right <$> liftIO (return $ show integer)

cd :: FilePath -> IO CommandLineInterpreterResult
cd filePath = do 
    setCurrentDirectory filePath
    return $ Right ""

cwd :: IO CommandLineInterpreterResult
cwd = liftIO $ Right <$> getCurrentDirectory 

echo :: String -> IO CommandLineInterpreterResult
echo message = do 
    putStrLn message
    return $ Right ""

