package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genMap: Gen[Map[Int, Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int, Int]), genMap)
  } yield m.updated(k, v)

  lazy val genHeap: Gen[H] = for {
    value <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(value, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min of two") = forAll { (a: Int, b: Int) =>
    findMin(insert(b, insert(a, empty))) == Math.min(a, b)
  }

  property("add and then delete") = forAll { (a: Int) =>
    deleteMin(insert(a, empty)) == empty
  }

  property("min on empty heap") = forAll { (heap: H) =>
    throws(classOf[NoSuchElementException]) {
      findMin(empty)
    }
  }

  def constructSorted(h: H): List[Int] =
    if (isEmpty(h)) List()
    else findMin(h) :: constructSorted(deleteMin(h))

  property("sorted") = forAll { (heap: H) =>

    val candidate = constructSorted(heap)
    val sortedCandidate = candidate.sorted
    candidate == sortedCandidate

  }

  property("minimum of melding") = forAll { (first: H, second: H) =>

    val heap1 = first
    val heap2 = second

    if (isEmpty(heap1) && isEmpty(heap2)) true
    else {
      val EmptyHeap = empty
      val minimum = (heap1, heap2) match {
        case (heap, EmptyHeap) => findMin(heap)
        case (EmptyHeap, heap) => findMin(heap)
        case (h1, h2) => Math.min(findMin(h1), findMin(h2))
      }

      minimum == findMin(meld(heap1, heap2))

    }

  }


  property("melding props") = forAll { (heap: H) =>

    val melded = meld(heap, heap)

    val EmptyHeap = empty

    @tailrec
    def recur(h1: H, h2: H): Boolean = h1 match {
      case EmptyHeap => true
      case h =>
        if (findMin(h1) < findMin(h2)) false
        else recur(deleteMin(h1), deleteMin(h2))
    }

    recur(heap, melded)

  }

  property("prop2") = forAll { (a: Int, h: H) =>
    if (isEmpty(h)) true
    else {
      val minH = findMin(h)
      val heap = insert(a, h)

      Math.min(a, minH) == findMin(heap)

    }
  }

  property("associativity") = forAll { (h11: H, h22: H) =>

    val any = 10

    val h1 = insert(any, h11)
    val h2 = insert(any, h22)

    val melded1 = meld(h1, h2)
    val melded2 = meld(h2, h1)

    constructSorted(melded1) == constructSorted(melded2)

  }

  property("prop3") = forAll { (heap: H) =>
    if (isEmpty(heap) || findMin(heap) == Int.MinValue) true
    else {
      val min = findMin(heap)
      val heap1 = insert(min - 1, heap)

      (findMin(heap1) == (min - 1)) && (findMin(deleteMin(heap1)) == min)
    }
  }

}
