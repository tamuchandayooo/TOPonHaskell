{-# LANGUAGE OverloadedStrings #-}

module BibTeXLibrarySpec (spec) where

import Test.Hspec
import BibTeXLibrary -- Import the generated module
import Data.BibTeX.Parser (parseBibTeX)
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Directory (removeFile, doesFileExist)
import System.IO.Temp (withSystemTempFile)

spec :: Spec
spec = do
  describe "BibTeXLibrary" $ do
    it "should list entries in library.bib file" $ do
      withSystemTempFile "library.bib" $ \tempPath _ -> do
        T.writeFile tempPath "entry1 = {Author = {John Doe}, Title = {Example}}\nentry2 = {Author = {Jane Smith}, Title = {Another Example}}"
        entries <- listEntries
        entries `shouldSatisfy` (`elem` ["entry1", "entry2"])

    it "should add an entry to library.bib file" $ do
      withSystemTempFile "library.bib" $ \tempPath _ -> do
        addEntry "entry3 = {Author = {Alice Johnson}, Title = {Yet Another Example}}"
        entries <- listEntries
        entries `shouldSatisfy` (`elem` "entry3")

    it "should search for an entry in library.bib file" $ do
      withSystemTempFile "library.bib" $ \tempPath _ -> do
        T.writeFile tempPath "entry1 = {Author = {John Doe}, Title = {Example}}\nentry2 = {Author = {Jane Smith}, Title = {Another Example}}"
        searchResults <- searchEntries "Example"
        searchResults `shouldSatisfy` (`elem` "entry1")

    it "should list an entry found by search" $ do
      withSystemTempFile "library.bib" $ \tempPath _ -> do
        T.writeFile tempPath "entry1 = {Author = {John Doe}, Title = {Example}}\nentry2 = {Author = {Jane Smith}, Title = {Another Example}}"
        searchResults <- searchEntries "Example"
        entries <- listEntries
        searchResults `shouldSatisfy` (`elem` "entry1")
        entries `shouldSatisfy` (`elem` "entry2")

    it "should handle no entries in library.bib file" $ do
      withSystemTempFile "library.bib" $ \tempPath _ -> do
        entries <- listEntries
        entries `shouldBe` []

    it "should handle error when reading library.bib file" $ do
      withSystemTempFile "library.bib" $ \tempPath _ -> do
        removeFile tempPath
        listEntries `shouldThrow` anyException

    it "should handle error when adding an entry to library.bib file" $ do
      withSystemTempFile "library.bib" $ \tempPath _ -> do
        removeFile tempPath
        addEntry "entry1 = {Author = {John Doe}, Title = {Example}}"
        listEntries `shouldThrow` anyException

    it "should handle error when searching for an entry in library.bib file" $ do
      withSystemTempFile "library.bib" $ \tempPath _ -> do
        removeFile tempPath
        searchEntries "Example" `shouldThrow` anyException