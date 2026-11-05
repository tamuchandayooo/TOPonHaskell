module BibTeXLibrarySpec (spec) where

import BibTeXLibrary (BibTeXEntry, saveEntry, listEntries, searchEntries)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, shouldThrow)
import Data.List (null)
import System.IO.Temp (withSystemTempFile)

spec :: Spec
spec = do
    describe "BibTeXEntry type" $ do
        it "can be created and stored" $ do
            let entry = BibTeXEntry "Key" "Author" "Title" "Journal" "Year" "Volume" "Number" "Pages" "Publisher" "Address" "Email" "URL"
            withSystemTempFile "library.bib" $ \path _ -> do
                saveEntry entry path
                contents <- readFile path
                contents `shouldSatisfy` (\s -> s /= "")

    describe "saveEntry function" $ do
        it "saves a BibTeX entry to a file" $ do
            let entry = BibTeXEntry "Key" "Author" "Title" "Journal" "Year" "Volume" "Number" "Pages" "Publisher" "Address" "Email" "URL"
            withSystemTempFile "library.bib" $ \path _ -> do
                saveEntry entry path
                contents <- readFile path
                contents `shouldSatisfy` (\s -> s /= "")

    describe "listEntries function" $ do
        it "lists all BibTeX entries in a file" $ do
            withSystemTempFile "library.bib" $ \path _ -> do
                saveEntry (BibTeXEntry "Key" "Author" "Title" "Journal" "Year" "Volume" "Number" "Pages" "Publisher" "Address" "Email" "URL") path
                entries <- listEntries path
                entries `shouldNotBe` []

    describe "searchEntries function" $ do
        it "searches for BibTeX entries containing a specific word" $ do
            let entry = BibTeXEntry "Key" "Author" "Title" "Journal" "Year" "Volume" "Number" "Pages" "Publisher" "Address" "Email" "URL"
            withSystemTempFile "library.bib" $ \path _ -> do
                saveEntry entry path
                results <- searchEntries "Title" path
                results `shouldSatisfy` (\xs -> not (null xs))

        it "returns empty list if no matching entries are found" $ do
            withSystemTempFile "library.bib" $ \path _ -> do
                results <- searchEntries "NonExistentWord" path
                results `shouldBe` []

        it "returns empty list if the file is empty" $ do
            withSystemTempFile "library.bib" $ \path _ -> do
                results <- searchEntries "Word" path
                results `shouldBe` []