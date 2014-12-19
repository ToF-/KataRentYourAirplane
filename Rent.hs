module Rent
where

type Time = Int
type Duration = Int
type Money = Double
type Order = (Time, Duration, Money)
type Node = (Time, [Order])
type Plan = [Node]

profit :: [Order] -> Money
profit = exploit . plan

plan :: [Order] -> Plan
plan [] = []
plan [o] = [(start o,[]),(end o, [o])] 
plan [o,o'] = [(start o,[]),(start o',[]),(end o, [o,o'])] 

exploit :: Plan -> Money
exploit [] = 0
exploit p = profitAt (fst n) 
    where
        n = maximum p
        profitAt :: Time -> Money
        profitAt t = case lookup t p of 
                        Just [] -> 0 
                        Just os -> maximum $ map (\o -> (profitAt (start o)) +(price o)) os

orders :: Node -> [Order]
orders = snd 

price :: Order -> Money
price (_,_,p) = p

start :: Order -> Time
start (s,_,_) = s

end :: Order -> Time
end (s,d,_) = s + d
