module Spec where

import Test.Hspec

main :: IO ()
main = hspec $ spec

kankeinaiKansu :: String -> String
kankeinaiKansu str = "hikisuu:" ++ str 

unagi :: Spec
unagi = describe "description" $ do
    it "is test 1" $
        1 + 1 `shouldBe` 2

unagi2 :: Spec
unagi = context "context" $ do
    specify "test 2" $
        1 + 1 `shouldSatisfy` (== 2)