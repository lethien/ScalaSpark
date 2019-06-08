
val data = List('a', 'a', 'a', 'b', 'b', 'c', 'a')

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
  }
}

pack(data)

def encode[T](xs: List[T]): List[(T, Int)] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (first, rest) = xs span (y => y == x)
    (x, first.size) :: encode(rest)
  }
}
// encode = pack(xs) map (ys => (ys.head, ys.size))

encode(data)