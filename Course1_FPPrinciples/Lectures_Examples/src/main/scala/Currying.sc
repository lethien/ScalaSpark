// MapReduce
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  if(a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
}
def fact(n: Int) = mapReduce(x=>x, (x,y)=>x*y, 1)(1, n)
fact(5)