import Data.Map
import Data.Bits
import Debug.Trace

convertStateAux :: Int -> Int -> [Int] -> Maybe [Int]
convertStateAux n 0 acc | ((n == 0) || (n == 1)) = Just (n:acc)
                     | True = Nothing
convertStateAux n s acc | ((n `rem` 4) < 3) = convertStateAux (n `div` 4) (s - 1) ((n `rem` 4):acc)
                     | True = Nothing
convertState n = convertStateAux n 3 []

convertToNumber [a, b, c, d] = (shift a 6) + (shift b 4) + (shift c 2) + d
convertToNumber _ = -1

transition (Just [1, _, _, _]) 2 = Nothing 
transition (Just [_, 1, 1, 1]) _ = Nothing
transition (Just [_, _, 1, 1]) 1 = Nothing
transition (Just [_, 2, _, _]) 2 = Nothing 
transition (Just [_, _, 2, _]) 2 = Nothing 
transition (Just [_, _, _, 2]) 2 = Nothing
transition (Just [1, 2, _, _]) _ = Nothing 
transition (Just [1, _, 2, _]) _ = Nothing 
transition (Just [1, _, _, 2]) _ = Nothing
transition (Just [_, 2, 2, _]) _ = Nothing 
transition (Just [_, _, 2, 2]) _ = Nothing 
transition (Just [_, 2, _, 2]) _ = Nothing 
transition (Just [0, 2, a, b]) c = Just [1, a, b, c]
transition (Just [a, _, c, d]) e = Just [a, c, d, e]
transition _ _ = Nothing

correctInitialsAux (-1) _ acc = acc
correctInitialsAux n Nothing acc = correctInitialsAux (n - 1) (transition (convertState (n-1)) 0) acc
correctInitialsAux n _ acc = correctInitialsAux (n - 1) (transition (convertState (n-1)) 0) (insert n 1 acc)
correctInitials = correctInitialsAux s (transition (convertState s) 0) empty
    where s = (shift 1 6) - 1

addInMapAux k v Nothing m = (insert k v m)
addInMapAux k v (Just pv) m = (insert k (v + pv) m)
addInMap k v m = addInMapAux k v (Data.Map.lookup k m) m

applyAux _ Nothing acc = acc
applyAux v (Just nk) acc = (addInMap (convertToNumber nk) v acc)
apply (k, v) t acc = applyAux v (transition (convertState k) t) acc

nextStep [] acc = acc
nextStep (h:tail) acc | True = nextStep tail (apply h 0 (apply h 1 (apply h 2 acc)))

count [] acc = acc 
count ((_,v):tail) acc = count tail (v + acc)

countPrizeStringsAux 0 acc = count (toList acc) 0
countPrizeStringsAux n acc = countPrizeStringsAux (n-1) (nextStep (toList acc) empty)
countPrizeStrings = countPrizeStringsAux 27 (correctInitials)

second (a, b) = ((convertState a), b)
main = print (countPrizeStrings)

