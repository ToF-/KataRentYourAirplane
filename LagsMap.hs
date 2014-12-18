module LagsMap
where
import Order
import qualified Data.Map as Map

type Arrival = (Price, [Order])
type Plan = Map.Map Time Arrival

plan :: [Order] -> Plan
plan = foldr insert Map.empty
    where
        insert :: Order -> Plan -> Plan 
        insert o p = Map.insertWith addOrder (arrival o) (0,[o]) 
                        $ Map.insertWith addOrder (start o) (0,[]) p
        addOrder :: Arrival -> Arrival -> Arrival
        addOrder (_,[])    (p,olds) = (p,olds)
        addOrder (_,[new]) (p,olds) = (p,new:olds) 

profit :: [Order] -> Price
profit [] = 0
profit os = fst (snd (last (Map.toList (foldr profit' p hours))))
    where 
          p :: Plan
          p = plan os
          hours :: [Time] 
          hours = Map.keys p
          profit' :: Time -> Plan -> Plan  
          profit' h p = p

