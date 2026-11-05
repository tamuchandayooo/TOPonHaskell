{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.FileOutput where

import Data.Text (Text,unpack)
import qualified Data.Text as T
import System.IO (withFile, IOMode(WriteMode), hSetEncoding, utf8, hPutStr)

fileOutput :: FilePath -> Text -> IO ()
fileOutput path text = 
    withFile path WriteMode $ \h -> do
        hSetEncoding h utf8
        hPutStr h (unpack (stripCodeFence text))

stripCodeFence :: Text -> Text
stripCodeFence input =
  let ls = T.lines (T.strip input)
      ls1 = case ls of
        (x:xs) | "```" `T.isPrefixOf` T.stripStart x -> xs
        _                                            -> ls
      ls2 = case reverse ls1 of
        (x:xs) | T.strip x == "```" -> reverse xs
        _                           -> ls1
  in T.strip (T.unlines ls2)