module Lib where 

import Prelude (IO, Int, Either(..), String, putStrLn, return, show, ($), (.), (<$>))
import System.Directory (getCurrentDirectory, setCurrentDirectory, listDirectory)
import System.IO (FilePath)
import Control.Monad.IO.Class (liftIO)

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


