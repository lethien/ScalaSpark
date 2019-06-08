import math.abs

class Rational(x: Int, y: Int) {
  // Check and throw IllegalArgumentException if fail
  require(y != 0, "denominator must be nonzero")

  // Define another constructor
  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int =
    if(b == 0) a else gcd(b, a % b)
  private val g = gcd(abs(x), abs(y))

  val numer = x / g
  val denom = y / g

  def < (other: Rational) = numer * other.denom < other.numer * denom

  def max(other: Rational) = if(this < other) other else this

  def + (other: Rational) = {
    new Rational(
      numer * other.denom + other.numer * denom,
      denom * other.denom
    )
  }

  def unary_- : Rational = new Rational(-numer, denom)

  def - (other: Rational) = this + -other

  override def toString = numer + "/" + denom
}

def r = new Rational(1, 2)
r.numer

-r

val x = new Rational(1, 3)
val y = new Rational(5, 8)
x + y