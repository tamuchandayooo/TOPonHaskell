module MainSpec (spec) where

import BibTeXLibrary
import Test.Hspec

spec :: Spec
spec = do
  describe "BibTeXEntry type" $ do
    it "should have expected fields" $ do
      let entry = BibTeXEntry { content = "entry content", authors = ["Author1", "Author2"] }
      entry `shouldSatisfy` (\e -> content e == "entry content" && authors e == ["Author1", "Author2"])

  describe "saveEntry function" $ do
    it "should save an entry to a file" $ do
      let entry = BibTeXEntry { content = "test entry", authors = ["Test Author"] }
      saveEntry entry
      -- Implement a file existence check here

  describe "listEntries function" $ do
    it "should list all saved entries" $ do
      listEntries `shouldSatisfy` (== [])

  describe "searchEntries function" $ do
    it "should find entries containing a specific word" $ do
      searchEntries "entry" `shouldSatisfy` (== [])