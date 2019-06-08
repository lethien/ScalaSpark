import math.abs

val tolerance = 0.0001
def isCloseEnough(a: Double, b: Double) = {
  abs((a - b) / a) / a < tolerance
}

// Fixed point x of a function is when x = f(x)
def findingFixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if(isCloseEnough(next, guess)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2


// For example, sqrt of x is a fixed point of (guess + x / guess) / 2
def sqrt(x: Double) =
  findingFixedPoint(averageDamp(y => x / y))(1)
sqrt(2)