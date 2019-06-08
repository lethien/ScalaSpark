// Greatest common divisor
def gcd(x: Int, y: Int): Int =
  if(y == 0) x else gcd(y, x % y)
gcd(21, 14)

// Factorial n!
def factorial(n: Int): Int = {
  def loop(acc: Int, n: Int): Int =
    if(n == 0) acc
    else loop(acc * n, n-1)

  loop(1, n)
}
factorial(5)

// Sum numbers from a to b, each applied the f function
def sum(f: Int => Int)(a: Int, b: Int): Int = {
  if(a > b) 0
  else f(a) + sum(f)(a + 1, b)
}
sum(x => x * x)(0, 2)