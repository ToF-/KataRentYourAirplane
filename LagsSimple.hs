module LagsSimple
where
import Order

lags :: [Order] -> Integer
lags [] = 0
lags (o:os) = max p1 p2
    where p1 = price o + lags (filter (\x -> x `follows` o) os)
          p2 = lags os
