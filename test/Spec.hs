module Spec where

import Test.Hspec

main :: IO ()
main = hspec $ unagi

kankeinaiKansu :: String -> String
kankeinaiKansu str = "hikisuu:" ++ str 

unagi :: Spec
unagi = describe "description" $ do
    it "is test 1" $
        (1 + 1) `shouldBe` 2
    it "is test 2" $
        (1 * 1) `shouldBe` 1

usagi :: Spec
usagi = describe "description2" $ do
    describe "description in description" $ do
        it "is test 3" $
            (2 + 2) `shouldBe` 4
        it "is test 4" $
            (2 * 2) `shouldBe` 4
    context "context in description" $ do
        it "is test 5" $
            (3 + 3) `shouldBe` 6
        it "is test 6" $
            (3 * 3) `shouldBe` 9

unagi2 :: Spec
unagi2 = context "context" $ do
    specify "test 2" $
        (1 + 1) `shouldSatisfy` (== 2)

unagi3 :: Spec
unagi3 = it "is test3" ((1 + 1) `shouldBe` 2)