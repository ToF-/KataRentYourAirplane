module Order
where
type Time = Integer
type Duration = Integer
type Price = Integer
type Order = (Time, Duration, Price)

price :: Order -> Price
price (_,_,p) = p

start :: Order -> Time
start (s,_,_) = s

duration :: Order -> Duration
duration (_,t,_) = t

arrival :: Order -> Time
arrival o = start o + duration o

follows :: Order -> Order -> Bool
follows o2 o1 = start o2 >= (start o1 + duration o1) 
