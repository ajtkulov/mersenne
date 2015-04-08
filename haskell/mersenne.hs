import Data.Set as Set
import Data.Hashable as H

mer p = 2^p - 1

next1 :: Integral a => a -> a -> a
next1 x p = (x ^ 2 - 2 + p) `mod` p


seq1 :: Integral a => a -> a -> Set a -> Set a
seq1 x p s = if (Set.member x s) 
       	   then
		s
           else 
 	   	seq1 (next1 x p) p (Set.insert x s)

cycleLength :: (Hashable a, Num a, Ord a) => a -> (a -> a) -> Set Int -> a -> a -> a
cycleLength elem func set len limit = if len > limit 
	    	      	      	then limit
	    	      	        else 
				  if (Set.member h set)
	    	   	    	then
					len
				else
					cycleLength newElem func (Set.insert h set) (len + 1) limit
				where
					h = H.hash elem
					newElem = func elem
					newHash = H.hash newElem
