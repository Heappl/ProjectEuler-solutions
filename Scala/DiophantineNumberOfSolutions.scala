import java.math.BigInteger
object DiophantineNumberOfSolutions {
    val maxN = 100000000
    
    val primesToMaxN = new Array[Int](maxN)
    val divisors = new Array[Int](maxN)
  
    def calculatePrimes() = {
        def markNonPrimes(prime: Int) {
            for (i <- prime * 2 to maxN by prime)
                primesToMaxN(i - 1) = prime
        }
        markNonPrimes(2)
        for (i <- 3 to Math.sqrt(maxN).asInstanceOf[Int] by 2)
            if (primesToMaxN(i - 1) == 0) markNonPrimes(i)
    }
    
    def calculateDivisors() = {
	    def calcDivisors(x: Int) : Int = {
	        val prime = primesToMaxN(x - 1)
	        if (prime == 0) return 3
	        var temp = x
	        var count = 0
	        while (temp % prime == 0) {
	            count += 1
	            temp /= prime
	        }
	        divisors(temp - 1) * (count * 2 + 1)
	    }
	    divisors(0) = 1
        for (i <- 2 to maxN)
            divisors(i - 1) = calcDivisors(i)
    }
    
    def divisorsFromFactors(leftPrimeFactors: List[(Int, Int)]): Int = {
        def multiple(acc: Int, elem: (Int, Int)) = elem match {
            case (_, power) => acc * (power * 2 + 1)
        }
        leftPrimeFactors.foldLeft(1)(multiple)
    }
    
    def next(primeFactors: List[(Int, Int)], soFar: Int): (List[(Int, Int)], Int, Int) = {
        if (primeFactors.isEmpty) return (List(), 0, 1)
        val (prime, power) = primeFactors.head
        val increase = divisorsFromFactors(primeFactors.tail) * soFar
        val (bestNext, bestIncrease, bestPrime) =
            if (power != 0) next(primeFactors.tail, soFar * (power * 2 + 1))
            else (primeFactors, 0, prime)
        val bestScore = (bestIncrease.toDouble) / (bestPrime.toDouble)
        val currentScore = (increase.toDouble) / (prime.toDouble)
        if (bestScore > currentScore) (primeFactors.head :: bestNext, bestIncrease, bestPrime)
        else ((prime, power + 1) :: primeFactors.tail, increase, prime)
    }
    
    def valueFromFactors(primeFactors: List[(Int, Int)]): BigInteger = {
        def multiple(acc: BigInteger, elem: (Int, Int)) = elem match {
            case (prime, power) => acc.multiply(BigInteger.valueOf(prime).pow(power))
        }
        primeFactors.foldLeft(BigInteger.valueOf(1))(multiple)
    }
    
    def divisorsFromFactors2(leftPrimeFactors: List[(Int, Int)]): Long = {
        def multiple(acc: Long, elem: (Int, Int)) = elem match {
            case (_, power) => acc * (power * 2 + 1)
        }
        leftPrimeFactors.foldLeft(1 toLong)(multiple)
    }
    
    def generateEmpty(primeFactors: List[(Int, Int)]): List[(Int, Int)] = {
        if (primeFactors.isEmpty) return primeFactors
        val (prime, _) = primeFactors.head
        (prime, 0) :: generateEmpty(primeFactors.tail)
    }
    
    def calcNextValue(prevValue: BigInteger, prime: Int, power: Int): BigInteger = {
        prevValue.divide(BigInteger.valueOf(prime).pow(power))
    }
    
    var bestValue = BigInteger.valueOf(10).pow(10000)
    var bestDivisors = Long.MaxValue
    
    def nextAux(primeFactors: List[(Int, Int)],
            	value: BigInteger): List[(Int, Int)] = {
        if (primeFactors.isEmpty) return List()
        val (prime1, power1) = primeFactors.head
        if (power1 > 42) return List()
        if (primeFactors.tail.isEmpty)
            return List((prime1, power1 + 1))
        val (prime2, power2) = primeFactors.tail.head
        val nextCandValue = value.multiply(BigInteger.valueOf(prime1))
        if ((power2 > power1) && (nextCandValue.compareTo(bestValue) <= 0))
        	(prime1, power1 + 1) :: primeFactors.tail
        else
        	(prime1, 0) :: nextAux(primeFactors.tail, calcNextValue(value, prime1, power1))
    }
    def next(primeFactors: List[(Int, Int)],
             value: BigInteger): List[(Int, Int)] = {
        val aux = nextAux(primeFactors.reverse, value).reverse
        if (aux.size != primeFactors.size) List()
        else aux
    }
    def findBest(primeFactors: List[(Int, Int)]) {
        var aux = primeFactors
        while (!aux.isEmpty) {
            val divisors = divisorsFromFactors2(aux)
            val value = valueFromFactors(aux)
            if ((divisors > 8000000) && (value.compareTo(bestValue) < 0)) {
                println(value + " " + divisors)
                bestDivisors = divisors
                bestValue = value
            }
            //println(aux + " " + divisors + " " + value)
            aux = next(aux, value)
        }
    }
    
	def main(args: Array[String]) {
	    var initial = List((2, 1), (3, 0), (5, 0), (7, 0), (11, 0), (13, 0),
	            (17, 0), (19, 0), (23, 0), (29, 0), (31, 0), (37, 0))
	    findBest(initial)
	}
}

