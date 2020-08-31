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

cycleLengthPrivate :: (Hashable a, Num a, Ord a) => a -> (a -> a) -> Set Int -> a -> a -> a
cycleLengthPrivate elem func set len limit = if len > limit 
	    	      	      	then limit
	    	      	        else 
				  if (Set.member h set)
	    	   	    	then
					len
				else
					cycleLengthPrivate newElem func (Set.insert h set) (len + 1) limit
				where
					h = H.hash elem
					newElem = func elem

cycleLength elem func limit = cycleLengthPrivate elem func empty 0 limit