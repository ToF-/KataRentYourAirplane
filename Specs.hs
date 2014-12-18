module Main
where
import Order
import LagsSimple as S
import LagsPerformante as P
import LagsMap as M
import Test.QuickCheck
import Test.Hspec
import qualified Data.Map as Map

orders = [(0,5,10),
          (3,7,14),
          (5,9,7),
          (6,9,8)]

main = do 
    hspec $ do
        describe "Lags Simple" $ do
            it "is 0 when list empty" $ do
                S.lags [] `shouldBe` 0

            it "is the order price when one order only" $ do
                S.lags [(0,7,13)] `shouldBe` 13

            it "is the best price when two overlapping orders" $ do
                S.lags [(0,7,13),
                        (3,8,15)] `shouldBe` 15


            it "is the sum of prices when two non overlapping orders" $ do
                S.lags [(0,7,13),
                        (8,15,7)] `shouldBe` 20

            it "is the best sum when given several orders" $ do
                S.lags [(0,5,10),
                        (3,7,14),
                        (5,9,7),
                        (6,9,8)] `shouldBe` 18

            it "works with a wild test case from the internet" $ do
                S.lags [(0  ,5 ,6 ),
                        (2  ,3 ,1 ),
                        (8  ,5 ,11 ),
                        (15 ,2 ,1 ),
                        (15 ,5 ,10 ),
                        (1  ,4 ,3 ),
                        (20 ,1 ,1 ),
                        (20 ,2 ,10 ),
                        (25 ,2 ,15 ),
                        (25 ,2 ,11 )] `shouldBe` 52

        describe "Lags Performante" $ do
            it "knows the list of each arrival time" $ do
                P.hours orders `shouldBe` 
                    [(0,[]),
                     (3,[]),
                     (5,[(0,5,10)]),
                     (6,[]),
                     (10,[(3,7,14)]),
                     (14,[(5,9,7)]),
                     (15,[(6,9,8)])]

            it "knows the list of initial prices" $ do
                P.initialPrices orders `shouldBe`  
                    Map.fromList [(0,0),(3,0),(5,0),(6,0)]

            it "can change an hour with a price, to the list of prices" $ do
                let ps = Map.fromList [(0,0),(5,0)]
                    ps'= addPrice 5 10 ps
                ps' `shouldBe` Map.fromList [(0,0),(5,10)]

            it "can add an hour with a price, to the list of prices" $ do
                let ps = Map.fromList [(0,0),(5,0)]
                    ps'= addPrice 10 14 ps
                ps' `shouldBe` Map.fromList [(0,0),(5,0),(10,14)]

            it "adds a price only if it's better" $ do
                let ps = Map.fromList [(0,0),(5,20)]
                    ps'= addPrice 5 10 ps 
                ps' `shouldBe` Map.fromList [(0,0),(5,20)]

            it "can update the list of prices from an order" $ do
                let ps = Map.fromList [(0,0),(5,10)]
                    ps' = addOrder 10 (5,9,7) ps
                ps' `shouldBe` Map.fromList [(0,0),(5,10),(14,17)]

            it "can update the list of prices from an order with best price at an arrival time" $ do
                let ps = Map.fromList [(0,0),(5,10)]
                    ps' = addOrder 10 (0,9,7) ps
                ps' `shouldBe` Map.fromList [(0,0),(5,10),(9,10)]

        describe "Lags on map" $ do
            it "should map the orders on a plan" $ do
                plan [] `shouldBe` Map.empty
                plan [(0,5,10)] `shouldBe` Map.fromList [(0,(0,[])),
                                                         (5,(0,[(0,5,10)]))]
                plan [(0,5,10),(3,2,12)] `shouldBe` 
                    Map.fromList [(0,(0,[])),
                                  (3,(0,[])),
                                  (5,(0,[(0,5,10),(3,2,12)]))]
                plan [(0,5,10),(0,7,12)] `shouldBe`
                    Map.fromList [(0,(0,[])),
                                  (5,(0,[(0,5,10)])),
                                  (7,(0,[(0,7,12)]))]

            it "should calculate the best profit" $ do
                profit [] `shouldBe` 0
                profit [(0,5,10)] `shouldBe` 10

    -- quickCheck (\orders -> P.lags orders == S.lags orders)

