import Data.List
import Debug.Trace

isRightAngle (x1, y1) (x2, y2) = x1*x2 == -y1*y2
mutuallyPrime (x, y) = (gcd x y) == 1
prod _ [] _ = []
prod [] (h:tail) s = prod s tail []
prod (h1:tail1) (h2:tail2) s = (h1,h2):(prod tail1 (h2:tail2) (h1:s))

countKicked n top (a, b) | (a * n > top) || (b * n > top) = 0
                         | True = next + curr
    where next = countKicked (n + 1) top (a, b)
          full = floor ((fromIntegral (n * a)) / (fromIntegral b))
          toTop = floor ((fromIntegral (top - (n * b))) / (fromIntegral a))
          curr = min full toTop

countTriangles = trivial + 2 * kicked
    where mpp = (filter mutuallyPrime (prod [1..50] [1..50] []))
          trivial = 50^2*3 
          kicked = foldl (+) 0 (map (countKicked 1 50) mpp)

main = print countTriangles

