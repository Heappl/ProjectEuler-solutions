import Debug.Trace

toString [] = ""
toString (h:tail) = (show h) ++ " " ++ (toString tail)

integerSolutionX n = round (aux4/8.0)
    where aux1 = sqrt(2)
          aux2 = (3.0-2.0*aux1)^n
          aux3 = (3.0+2.0*aux1)^n
          aux4 = 2.0*aux2+aux1*aux2+2.0*aux3-aux1*aux3+4.0

integerSolutionY n = round (aux4/4.0)
    where aux1 = sqrt(2)
          aux2 = (3.0-2.0*aux1)^n
          aux3 = (3.0+2.0*aux1)^n
          aux4 = -aux2-aux1*aux2-aux3+aux1*aux3+2.0

integerSolution n = ((integerSolutionX n), (integerSolutionY n))

findFirstSolutionOverAux top n solGener | y >= top = (x, y)
                                        | True = findFirstSolutionOverAux top (n+1) solGener
    where (x, y) = solGener n 


main = print (findFirstSolutionOverAux (10^12) 4 integerSolution)

