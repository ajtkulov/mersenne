import System.Random

primes = 2 : filter isPrime [3, 5..]

isPrime n = length (primeFactors n) == 1

primeFactors n = factor n primes
     where 
         factor n (p : ps) 
               | p * p > n = [n]
               | n `mod` p == 0 = p : factor (n `div` p) (p : ps)
               | otherwise = factor n ps

pow a 0 m = 1
pow a 1 m = a `mod` m
pow a b m = ((pow a (b `div` 2) m) ^ 2 * pow a (b `mod` 2) m) `mod` m

s m = iterate (\x -> (x ^ 2 - 2) `mod` m) 4

ss start m = iterate (\x -> (x ^ 2 - 2) `mod` m) start

mer p = 2 ^ p - 1

isMersenne p = (s $ mer p) !! p - 2 == 0

qsort [] = []
qsort (x : xs) = qsort[y | y <- xs, y < x] ++ [x] ++ qsort[y | y <- xs, y >= x]

unique a = unique1 $ qsort a 
     where
         unique1 [] = []
         unique1 (x : y : xs) | x == y = unique1(x : xs)
         unique1 (x : xs) = [x] ++ unique1(xs)

isPP p = (2 ^ ((p + 1) `div` 2) , mer p)

isPP1 x p = (pow x ((p + 1) `div` 2) $ mer p, mer p)

solve x p = pow x ((m + 1) `div` 4) m
         where m = mer p

solve1 x p = (z, m - z)
       where z = solve x p
             m = mer p

check x p = (pow (solve x p) 2 $ mer p) == x || (pow (mer p - (solve x p)) 2 $ mer p) == x

firstMersenne = [3,5,7,13,17,19,31,61,89,107,127,521,607,1279,2203,2281,3217]

allMersenne = [3,5,7,13,17,19,31,61,89,107,127,521,607,1279,2203,2281,3217,4253,4423,9689,9941,11213,19937,21701,23209,44497,86243,110503,132049,216091,756839,859433,1257787,1398269,2976221,3021377,6972593,13466917,20996011,24036583,25964951,30402457,32582657,37156667,42643801,43112609]

primeNonMersenne = [11, 23, 29, 37, 41, 43, 47, 53, 59]

check2 p = (check (sol + 2) p, check (mer p - sol + 2) p)
              where sol = solve 2 p
check22 p = check (sol + 2) p
              where sol = solve 2 p

check2Map = zip list $ map (check2) list
          where list = primes

check2Filter = filter (\x -> snd(snd x) || fst(snd x)) $ check2Map

order g m = 2 + length (takeWhile (\x -> not(x == 1 || x == g)) $ iterate (\x -> x * g `mod` m) $ g*g `mod` m)

group g m = [1, g] ++ (takeWhile (\x -> not(x == 1 || x == g)) $ iterate (\x -> x * g `mod` m) $ g*g `mod` m)

q e m = filter (\x -> elem e (snd x)) $ zip list $ map (\x -> group x m) list
          where list = [1..m-1]

inverse a p = pow a (p - 2) p

check_ p = inverse x1 mers + inverse x2 mers
           where mers = mer p
                 sol = solve 2 p
                 x1 = sol + 2
                 x2 = mers - sol + 2

check__ p = check_ p == mer p + 2

check__map = zip list $ map (check__) list
           where list = primes

check__filter = filter (\x -> snd x) $ check__map 

inverse1 p = mers - pow 2 ((p - 1) `div` 2) mers + 1
             where mers = mer p




checkMain p = x2 * inv2 `mod` mers == 1
            where mers = mer p
                  sol = solve 2 p
                  x1 = sol + 2
                  x2 = mers - sol + 2
                  inv1 = inverse1 p
                  inv2 = mers + 2 - inv1

genRand = tail $ map fst $ iterate (\x -> random (snd x) :: (Integer, StdGen)) $ (0, mkStdGen 42)
genM m = map (\x -> (abs x + 1) `mod` m) genRand

rabin p = and $ map (\x -> (pow x (p - 1) p) == 1) (take 25 $ genM p)
genPrimes x = map (fst) $ filter (\x -> snd x) $ zip list $ map (rabin) list
         where list = [x, x + 2 ..]


solveEq f p = map (fst) $ filter(\x -> snd x) $ zip list $ map (\x -> f x `mod` p == 0) list
            where list = [0..p-1]



zzz k = zip  list $ map (\x -> length $ group x $ mer k) list
          where list = [2.. (mer k) - 1]


qqq = filter (\x -> not (isPrime x)) www
          where www = map (fst) zz
                zz = filter (\x -> fst x == snd x) $ zip list $ map (\z -> sum $ solveEq (\x -> x*x*x - 1) z) list
                list = filter (\x -> x `mod` 3 == 1) [1..]
          --where list = filter (\x -> x `mod` 3 == 1) primes

smallGroup m = filter (\x -> snd x < 20) $ zip groupList $ map (\x -> order x num) groupList
             where num = mer m
                   groupSize = num - 1
                   groupList = [1..groupSize]


modmod a b = if a < 0 then 
       modmod (a + b) b
       else
       a `mod` b

sq1 p = (roots, ((snd roots) * (fst roots)) `mod` m)
    where
    roots = ((-2 + sqrt2) `modmod` m, (-2 - sqrt2) `modmod` m)
    sqrt2 = solve 2 p
    m = mer p


mcheck p len = map (\x -> check (x * x `mod` (mer p)) p) [ 1.. len]

fcheck p len = filter (\x -> check (x * x `mod` (mer p)) p) [ 1.. len]

some p = (2 ^ (2 ^ (p - 2)) - 1)