module Infrastructure.Logger where

import Data.Time
import Data.Text (Text, unpack)
import System.Directory (doesFileExist)
import System.IO (writeFile,appendFile)

logfile = "log.log"

crtLogFileIfMissing :: IO ()
crtLogFileIfMissing = do
    exist <- doesFileExist logfile
    if exist
        then return ()
        else writeFile logfile ""

putInfoLog :: String -> IO ()
putInfoLog m = do
    crtLogFileIfMissing
    now <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
    let content = "[" ++ timestamp ++ "][INFO]:" ++ m ++ "\n"
    putStr             content
    appendFile logfile content

putErrorLog :: String -> IO ()
putErrorLog m = do
    crtLogFileIfMissing
    now <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
    let content = "[" ++ timestamp ++ "][ERROR]:" ++ m ++ "\n"
    putStr             content
    appendFile logfile content

putWarnLog :: String -> IO ()
putWarnLog m = do
    crtLogFileIfMissing
    now <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
    let content = "[" ++ timestamp ++ "][WARN]:" ++ m ++ "\n"
    putStr             content
    appendFile logfile content

putDebugLog :: String -> IO ()
putDebugLog m = do
    crtLogFileIfMissing
    now <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
    let content = "[" ++ timestamp ++ "][DEBUG]:" ++ m ++ "\n"
    putStr             content
    appendFile logfile content

putInputLog :: String -> IO ()
putInputLog m = do
    crtLogFileIfMissing
    now <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
    let content = "[" ++ timestamp ++ "] >> " ++ m ++ "\n"
    appendFile logfile content