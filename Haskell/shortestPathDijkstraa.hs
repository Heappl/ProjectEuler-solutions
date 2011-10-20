import Data.Map
import Data.List
import Debug.Trace

maxInt = 2^30

--- Dijkstraa algorithm
newDists :: Map (Int, Int) Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
newDists _ _ _ [] acc = sort(acc)
newDists gr d p ((dh, ph):tail) acc = newDists gr d p tail ((newDist, ph):acc)
    where distCand = (findWithDefault maxInt (p, ph) gr) + d
          newDist = min distCand dh

shortestPathAux _ [] acc = acc
shortestPathAux gr ((d, p):tail) acc = shortestPathAux gr (newDists gr d p tail []) ((p, d):acc)
shortestPath gr p vertices = shortestPathAux gr initial []
    where unreachable = Data.List.map (\v -> (maxInt, v)) (Data.List.filter (\v -> v > 1) vertices)
          initial = ((findWithDefault maxInt (0, 1) gr), 1):unreachable

--- graph generator

generateVertex w r c = r * w + c + 1
generateRowVertices genVertex w c | w == c = []
                                  | True = (genVertex c):(generateRowVertices genVertex w (c + 1))
generateVertices h w r | h == r = []
                       | True = (generateRowVertices (\c -> generateVertex w r c) w 0):(generateVertices h w (r + 1))

generateRight [_] [_] = [] 
generateRight (_:w2:wtail) (v1:v2:vtail) = ((v1, v2), w2):(generateRight (w2:wtail) (v2:vtail))
generateLeft [_] [_] = [] 
generateLeft (w1:wtail) (v1:v2:vtail) = ((v2, v1), w1):(generateLeft wtail (v2:vtail))
generateRows [] [] = [] 
generateRows (wrow:wtail) (vrow:vtail) = (generateRight wrow vrow) ++ (generateLeft wrow vrow) ++ (generateRows wtail vtail)
generateGraph weights = fromList (rows ++ cols ++ [((0, 1), firstWeight)])
    where firstRow:_ = weights
          firstWeight:_ = firstRow
          height = length weights
          width = length firstRow
          vertices = generateVertices height width 0
          rows = generateRows weights vertices
          cols = generateRows (transpose weights) (transpose vertices)

--- file read

main = print(shortestPath gr 0 vertices)
    where t = [[131, 673, 234, 103,  18],
               [201,  96, 342, 965, 150],
               [630, 803, 746, 422, 111],
               [537, 699, 497, 121, 956],
               [805, 732, 524,  37, 331]]
          gr = generateGraph t
          vertices = (foldl (++) [] (generateVertices 5 5 0))

