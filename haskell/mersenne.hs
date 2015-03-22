import Data.Set as Set

mer p = 2^p - 1

next1 :: Integral a => a -> a -> a
next1 x p = (x ^ 2 - 2 + p) `mod` p


seq1 :: Integral a => a -> a -> Set a -> Set a
seq1 x p s = if (member x s) 
       	   then
		s
           else 
 	   	seq1 (next1 x p) p (insert x s)