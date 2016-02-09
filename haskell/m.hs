--import Data.Set as Set
--import Data.Hashable as H

mer p = 2^p - 1

f x p = (x ^ 2 + x - s + m) `mod` m
    where m = mer p
          s = 2^(p - 2)

find p = filter (\x -> f x p == x - 1) [0..m]
    where m = mer p
