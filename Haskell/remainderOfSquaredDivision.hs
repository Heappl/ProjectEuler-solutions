import Debug.Trace

toString [] = ""
toString (h:tail) = (show h) ++ " " ++ (toString tail)

findGeneratedByPow _ 1 _ acc = (1:acc)
findGeneratedByPow g curr m acc | True = findGeneratedByPow g ((g * curr) `rem` m) m (curr:acc)

sumDuple m (a, b) = (a + b) `rem` m
mergeLists m a b = map (sumDuple m) (zip a b)

getEquallyLengthedSecond first second | length(second) == length(first) = second
                                      | True = second ++ second
getMaxRemainder a = maximum (mergeLists mod firstGenerated secondEquallyLengthed)
    where mod = a * a
          firstGenerated = findGeneratedByPow (a-1) (a-1) mod []
          secondGenerated = findGeneratedByPow (a+1) (a+1) mod []
          secondEquallyLengthed = getEquallyLengthedSecond firstGenerated secondGenerated

getSumOfMaxRemainder 2 = 0
getSumOfMaxRemainder a = (getMaxRemainder a) + (getSumOfMaxRemainder (a-1))

main = print (getSumOfMaxRemainder 1000) 

