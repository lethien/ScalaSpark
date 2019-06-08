import math.Ordering

def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if(n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x::xremains, y::yremains) => {
        if(ord.lt(x, y)) x::merge(xremains, ys)
        else y::merge(xs, yremains)
      }
    }

    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

val nums = List(2, -4, 5, -10)
msort(nums)

val fruits = List("apple", "orange", "banana")
msort(fruits)