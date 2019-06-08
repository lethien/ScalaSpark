// abstract class Inset represent a binary tree
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}

// class NonEmpty represent a non-empty node
class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def incl(x: Int) = {
    if(x > elem) new NonEmpty(elem, left, right incl x)
    else if(x < elem) new NonEmpty(elem, left incl x, right)
    else this
  }

  override def contains(x: Int) = {
    if(x > elem) right contains x
    else if(x < elem) left contains x
    else true
  }

  override def toString() = "{" + left + elem + right + "}"
}

// Singleton object Empty represent an empty node
// Singleton by using object instead of class
object Empty extends IntSet {
  override def incl(x: Int) = new NonEmpty(x, Empty, Empty)

  override def contains(x: Int) = false

  override def toString() = "."
}

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4