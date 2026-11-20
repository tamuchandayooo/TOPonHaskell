{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.FileManager where

import qualified Data.Text as T
import System.IO (withFile, IOMode(WriteMode), hSetEncoding, utf8, hPutStr)
import qualified Data.Text.IO as T

insertLine :: Int -> T.Text -> T.Text -> T.Text
insertLine row newLine txt =
    let ls = T.lines txt
        (before, after) = splitAt (row - 1) ls
    in T.intercalate "\n" (before ++ [newLine] ++ after)

insertLineInFile :: Int -> T.Text -> FilePath -> IO ()
insertLineInFile row newLine path = do
  txt <- T.readFile path
  let newtxt = insertLine row newLine txt
  fileOutput path newtxt

fileOutput :: FilePath -> T.Text -> IO ()
fileOutput path text = 
    withFile path WriteMode $ \h -> do
        hSetEncoding h utf8
        hPutStr h (T.unpack (stripCodeFence text))

stripCodeFence :: T.Text -> T.Text
stripCodeFence input =
  let ls = T.lines (T.strip input)
      ls1 = case ls of
        (x:xs) | "```" `T.isPrefixOf` T.stripStart x -> xs
        _                                            -> ls
      ls2 = case reverse ls1 of
        (x:xs) | T.strip x == "```" -> reverse xs
        _                           -> ls1
  in T.strip (T.unlines ls2)