module Spec where

import Test.Hspec

main :: IO ()
main = hspec $ unagi

kankeinaiKansu :: String -> String
kankeinaiKansu str = "hikisuu:" ++ str 

unagi :: Spec
unagi = describe "description" $ do
    it "is test 1" $
        1 + 1 `shouldBe` 2

unagi2 :: Spec
unagi2 = context "context" $ do
    specify "test 2" $
        1 + 1 `shouldSatisfy` (== 2)

unagi3 :: Spec
unagi3 = it "is test" ((1 + 1) `shouldBe` 2)