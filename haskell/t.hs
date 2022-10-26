-- import Data.Set (Set)

t n = 3 * 2 ^ n + 1


seq1 cur m = cur ^ 2 `mod` m

pos :: Int -> [Integer]
pos n = take n $ iterate (\t -> seq1 t m) 2 
  where m = t n 


test :: Int -> Bool
test n = elem 1 elements || slidingApprox(elements)
  where
    elements = pos n

slidingApprox :: [Integer] -> Bool
slidingApprox [] = False
slidingApprox [_] = False
slidingApprox (x : x1 : xs) = abs(x - x1) == 1 || slidingApprox(x1 : xs)


testS n = testSliding 4 2 (t n) 1 n

--testSliding cur prev m step n = 
testSliding :: Integer -> Integer -> Integer -> Int -> Int -> Bool
testSliding 1 _ _ _ _ = True
testSliding a b _ _ _ | abs (a - b) == 1 = True
testSliding cur prev m step n | step == n = False
testSliding cur prev m step n = testSliding (seq1 cur m) cur m (step + 1) n



-- https://oeis.org/search?q=13%2C+97%2C+193%2C+769%2C+12289&sort=&language=english&go=Search  

pp :: [Int]
pp = [1, 2, 5, 6, 8, 12, 18, 30, 36, 41, 66, 189, 201, 209, 276, 353, 408, 438, 534, 2208, 2816, 3168, 3189, 3912, 20909, 34350, 42294, 42665, 44685, 48150, 54792, 55182, 59973, 80190, 157169, 213321, 303093, 362765, 382449, 709968, 801978, 916773, 1832496, 2145353]

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs
