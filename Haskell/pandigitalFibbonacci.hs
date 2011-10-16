import Data.Bits
import Debug.Trace

-- find only fibbonacci numbers with pandigital ending
-- calculate fibbonacci based on index from sqrt formula
-- among calculated find one with pandigital beginning

digits :: Integer -> [Int] -> [Int]
digits 0 acc = acc 
digits n acc = digits (n `div` 10) (fromIntegral (n `mod` 10) : acc)

hasPandigitalBeginning _ 511 = True
hasPandigitalBeginning [] acc = False 
hasPandigitalBeginning (0:_) acc = False 
hasPandigitalBeginning (h:tail) acc | ((shift 1 (h - 1)) .&. acc :: Integer) > 0 = False
                                    | otherwise = hasPandigitalBeginning tail ((shift 1 (h - 1)) .|. acc)
     
pandigital _ 511 = True
pandigital 0 _ = False
pandigital n acc | (node .&. acc :: Integer) > 0 = False
                 | otherwise = pandigital (n `div` 10) (node .|. acc)
    where node = (shift 1 ((n `mod` 10) - 1))    

multiply [a11, a12, a21, a22] [b11, b12, b21, b22] = [a11*b11 + a21*b12, a11*b12 + a12*b22, a21*b11 + a22*b21, a21*b12+a22*b22]
fibbMatrixForm 0 = [0, 1, 1, 0] 
fibbMatrixForm 1 = [1, 1, 1, 0] 
fibbMatrixForm n = multiply (fibbMatrixForm half) (fibbMatrixForm (n - half))
    where half = n `div` 2
fibbonacci n = a
    where [a, _, _, _] = (fibbMatrixForm (n - 1))

frontPandigital n = hasPandigitalBeginning (digits fibb []) 0
    where fibb = fibbonacci n

findPandigitalFibbonacci pp p n | (isPandigital && isFrontPandigital) = n 
                                | (not isPandigital) = findPandigitalFibbonacci p fibb (n + 1)
                                | trace((show n)) True = findPandigitalFibbonacci p fibb (n + 1)
    where isPandigital = pandigital fibb 0 
          isFrontPandigital = frontPandigital n
          fibb = (pp + p) `mod` 1000000000

main = print(findPandigitalFibbonacci 1 1 3)
