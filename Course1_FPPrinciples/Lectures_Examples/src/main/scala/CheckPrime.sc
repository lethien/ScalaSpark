
def isPrime(n: Int) = (2 until n) forall (d => n % d != 0)

isPrime(5)
isPrime(8)
isPrime(1)

val n = 7

(1 until n) flatMap (i =>
  (1 until i) map (j => (i, j))) filter (pair =>
    isPrime(pair._1 + pair._2))