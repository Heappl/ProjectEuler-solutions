import Debug.Trace

toString [] = ""
toString (h:tail) = (show h) ++ " " ++ (toString tail)

calcDist k l = k * k + k * l + l * l
calcDiff k l = 3 * (2 * k + 1 + l)

countOnCircle dist k l acc | (k == 0) = acc
                           | (dist > (calcDist k l)) = countOnCircle dist k (l+1) acc
                           | (dist < (calcDist k l)) = countOnCircle dist (k - 1) l acc
                           | True = countOnCircle dist k (l+1) (acc+1)

countOnCircleInit i = countOnCircle (calcDist i i) i (i+1) 0

countOnCircles i m | (i == 0) = m
                   | (trace (toString [i, hexesSym])) True = countOnCircles (i-1) (max hexes m) 
    where hexesSym = countOnCircleInit i
          hexes = 6 + 12 * hexesSym

isPrime :: Integer -> Integer -> Bool
isPrime n k | (k * k > n) = True
            | (n `mod` k == 0) = False
            | True = isPrime n (k+2)

countNumbers :: Integer -> Integer -> Integer
countNumbers n acc | n < 3 = acc 
                   | (not (isPrime n 3)) = countNumbers (n-2) acc
                   | (n `mod` 3 /= 1) && (hexes == 0) = countNumbers (n-2) acc
                   | (n `mod` 3 == 1) && (hexes == 1) = countNumbers (n-2) (acc+1)
                   | (trace (toString [n, hexes])) True = countNumbers (n-2) (if (hexes == 0) then acc else (acc+1))
    where hexes = countOnCircleInit n 

findPrimes top acc | (isPrime top 3) && (top `rem` 3 == 1) = findPrimes (top-2) (top:acc)
                   | top >= 5 = findPrimes (top-2) acc
                   | (trace "findPrimes finished") True = acc

countCombs2 :: Integer -> Integer -> [Integer] -> Integer -> [Integer]
countCombs2 _ _ [] _ = []
countCombs2 fp sp (h:tail) top | (fp*fp+sp*sp+h < top) = (h:tail)
                               | True = countCombs2 fp sp tail top

countCombs1 :: Integer -> [Integer] -> [Integer] -> Integer -> Integer -> Integer
countCombs1 fp (sp:tp:tail) rprimes top acc | (fp*fp+sp*sp+tp > top) = acc
                                            | True = countCombs1 sp (tp:tail) nrprimes top (acc + (fromIntegral (length nrprimes)))
    where nrprimes = countCombs2 fp sp rprimes top
countCombs1 _ _ _ _ acc = acc

countCombs :: [Integer] -> [Integer] -> Integer -> Integer -> Integer
countCombs (fp:sp:tp:tail) rprimes top acc | (fp*fp+sp*sp+tp > top) = acc
                                           | True = countCombs (sp:tp:tail) nrprimes top (countCombs1 fp (sp:tp:tail) rprimes top acc)
    where nrprimes = countCombs2 fp sp rprimes top
countCombs _ _ _ acc = acc

--main = print (countOnCircles 100 0) 
--main = print (countOnCircleInit (13*7*19*7*37*43)) 
--main = print (countOnCircleInit 60379181) 
--main = print (countNumbers 100001 0) 
main = print (countCombs primes rprimes (5*10^11) 0)
    where top = 603791 --81
          primes = findPrimes top []
          rprimes = reverse primes

