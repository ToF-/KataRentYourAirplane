module Main
where 
import Test.Hspec
import Rent

main = hspec $ do
    describe "profit" $ do
        it "is zero when there are no orders" $ do
            profit [] `shouldBe` 0.0

        it "is the price of the order when there's only one order" $ do
            profit [(0,5,10.0)] `shouldBe` 10.0

        it "is the best price between two orders ending at same time" $ do
            profit [(0,5,10.0),(1,4,11.0)] `shouldBe` 11.0

        it "is the best price between two overlapping orders" $ do
            profit [(0,5,7.0),(2,3,10.0)] `shouldBe` 10.0
