package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      e <- arbitrary[A]
      h <- genHeap
    } yield insert(e, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minOf2") = forAll { (a: Int, b: Int) =>
    val min = if(a < b) a else b
    val h = insert(b, insert(a, empty))
    findMin(h) == min
  }

  property("isEmpty") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("orderOfMins") = forAll { (h: H) =>
    if(isEmpty(h)) true
    else {
      def removeItemsAreInOrder(heap: H, previousMin: A): Boolean = {
        if(isEmpty(heap)) true
        else {
          val min = findMin(heap)
          if(previousMin <= min) removeItemsAreInOrder(deleteMin(heap), min)
          else false
        }
      }
      removeItemsAreInOrder(h, findMin(h))
    }
  }

  property("minOfMelding") = forAll { (h1: H, h2: H) =>
    val h1Min = if (isEmpty(h1)) 0 else findMin(h1)
    val h2Min = if (isEmpty(h2)) 0 else findMin(h2)
    val h1h2Min = {
      val melded = meld(h1, h2)
      if (isEmpty(melded)) 0 else findMin(melded)
    }
    h1h2Min == h1Min || h1h2Min == h2Min
  }

  property("reservedOfMelding") = forAll { (h1: H, h2: H) =>
    def checkMelding(melded: H, h1: H, h2: H): Boolean = {
      val emptyH1H2 = isEmpty(h1) && isEmpty(h2)
      if(isEmpty(melded)) emptyH1H2
      else {
        if(!isEmpty(h1) && findMin(h1) == findMin(melded)) checkMelding(deleteMin(melded), deleteMin(h1), h2)
        else if(!isEmpty(h2) && findMin(h2) == findMin(melded)) checkMelding(deleteMin(melded), h1, deleteMin(h2))
        else false
      }
    }
    checkMelding(meld(h1, h2), h1, h2)
  }
}
