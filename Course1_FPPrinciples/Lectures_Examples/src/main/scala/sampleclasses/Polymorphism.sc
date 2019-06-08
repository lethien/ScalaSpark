import java.util.NoSuchElementException

// Trait is similar to Java 8's interface
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty = false

  override def toString = head + " "
}

class Nil[T] extends List[T] {
  override def isEmpty = true

  override def head: Nothing = throw new NoSuchElementException("Nil.head")

  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

def singleton[T](elem: T) = new Cons(elem, new Nil[T])

singleton(1)
singleton(true)