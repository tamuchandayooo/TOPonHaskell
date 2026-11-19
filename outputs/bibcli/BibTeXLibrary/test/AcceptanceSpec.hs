{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Bible (BibTeXEntry)
import qualified Data.Bible as Bible
import Data.ByteString (readFile, writeFile)
import Data.List (intersect)
import qualified Data.Text as T
import Test.Hspec
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath (joinPath, takeFileName)
import System.IO.Temp (withSystemTempDir)

main :: IO ()
main = hspec acceptanceTests

acceptanceTests :: Spec
acceptanceTests = do
  describe "When the program is invoked with the list option" $ do
    it "all the entries saved on the file should be returned" $ do
      withSystemTempDir "bibtex-library" $ \tempDir -> do
        let libraryFile = joinPath [tempDir, "library.bib"]
        writeFile libraryFile ""
        let entry1 = Bible.BibTeXEntry "entry1" "Title 1" ["Author 1"] "2022" "Abstract 1"
        let entry2 = Bible.BibTeXEntry "entry2" "Title 2" ["Author 2"] "2023" "Abstract 2"
        Bible.saveEntry entry1 libraryFile
        Bible.saveEntry entry2 libraryFile
        entries <- Bible.listEntries libraryFile
        entries `shouldBe` [entry1, entry2]

  describe "When the program is invoked with the add option passing the path of a file containing a BibTEX entry" $ do
    it "the entry should be stored on the library.bib file" $ do
      withSystemTempDir "bibtex-library" $ \tempDir -> do
        let libraryFile = joinPath [tempDir, "library.bib"]
        writeFile libraryFile ""
        let entry = Bible.BibTeXEntry "entry3" "Title 3" ["Author 3"] "2024" "Abstract 3"
        tempFile <- takeFileName libraryFile
        writeFile tempFile $ Bible.entryToString entry
        Bible.addEntry tempFile libraryFile
        entries <- Bible.listEntries libraryFile
        entries `shouldBe` [entry]

  describe "When a BibTEX entry is saved on library.bib file, it should be returned if the program is invoked with the search option with a word from the BibTEX entry" $ do
    it "the entry should be returned" $ do
      withSystemTempDir "bibtex-library" $ \tempDir -> do
        let libraryFile = joinPath [tempDir, "library.bib"]
        writeFile libraryFile ""
        let entry = Bible.BibTeXEntry "entry4" "Title 4" ["Author 4"] "2025" "Abstract 4"
        Bible.saveEntry entry libraryFile
        entries <- Bible.searchEntries "Title 4" libraryFile
        entries `shouldBe` [entry]

  describe "When a BibTEX entry is saved on library.bib file, it should be returned among the entries returned if the program is invoked with the list option" $ do
    it "the entry should be returned" $ do
      withSystemTempDir "bibtex-library" $ \tempDir -> do
        let libraryFile = joinPath [tempDir, "library.bib"]
        writeFile libraryFile ""
        let entry1 = Bible.BibTeXEntry "entry5" "Title 5" ["Author 5"] "2026" "Abstract 5"
        let entry2 = Bible.BibTeXEntry "entry6" "Title 6" ["Author 6"] "2027" "Abstract 6"
        Bible.saveEntry entry1 libraryFile
        Bible.saveEntry entry2 libraryFile
        entries1 <- Bible.listEntries libraryFile
        entries2 <- Bible.searchEntries "Title 5" libraryFile
        intersect entries1 entries2 `shouldBe` [entry1]

  describe "Cleanup" $ do
    it "removes the temporary directory and all its contents" $ do
      withSystemTempDir "bibtex-library" $ \_ -> do
        createDirectoryIfMissing True "/path/to/temp/dir" -- Replace with actual path
        removeDirectoryRecursive "/path/to/temp/dir" -- Replace with actual path