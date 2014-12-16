module LagsPerformante
where

import Order
import Data.List (sort,nub)
import qualified Data.Map as Map

type Prices = Map.Map Time Price
lags :: [Order] -> Integer
lags _ = -1

type Node = (Time,[Order])

hours :: [Order] -> [Node]
hours os = 
    let hs = nub (sort ((map (\o -> start o + duration o) os) ++ (map start os)))
    in [ (h,[o | o <- os, arrival o == h]) | h <- hs] 

initialPrices :: [Order] -> Prices 
initialPrices = Map.fromList . map (\o -> (start o, 0))

addPrice :: Time -> Price -> Prices -> Prices
addPrice t p  = Map.alter add t  
    where add :: Maybe Price -> Maybe Price
          add (Just i) 
            | i < p     = Just p
            | otherwise = Just i
          add Nothing   = Just p

addOrder :: Price -> Order -> Prices -> Prices 
addOrder m o ps = 
    let p = max m ((ps Map.! start o) + price o)
    in addPrice (arrival o) p ps
{--

 pour connaître le meilleur prix à une heure:
    le max parmi:
    les prix des heures précédentes
    les prix des vols arrivant à cette heure + prix de départ du vol
calculer le prix de la dernière heure 
--}
 


