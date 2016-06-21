package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("min2") = forAll { (a: Int, b: Int) =>
    def min(a: Int, b: Int) = if (a < b) a else b
    val h = insert(a, insert(b, empty))
    findMin(h) == min(a, b)
  }
  
  property("min3") = forAll { l: List[Int] =>
    def insertAll(h: H, l: List[Int]): H = 
      if (l.isEmpty) h else insertAll(insert(l.head, h), l.tail)
      
    if (l.length <= 1) true else {  
      val lsrt = l.sorted
      val h = insertAll(empty, lsrt)
      findMin(deleteMin(h)) == l.sorted.tail.head
    }
  }
  
  property("delete1") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }
  
  property("min4") = forAll { h: H =>
    def check(h: H, min: Int): Boolean = {
      if (isEmpty(h)) true else {
        val curMin = findMin(h)
        if (curMin < min) false else check(deleteMin(h), curMin) 
      }
    }
    check(h, Int.MinValue)
  }
  
  property("meld1") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val min = findMin(meld(h1, h2))
    min == findMin(h1) || min == findMin(h2)
  }
  

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  lazy val genList: Gen[List[Int]] = for {
    v <- arbitrary[Int]
    l <- oneOf(const(List()), genList)
  } yield v :: l
  
  implicit lazy val arbList: Arbitrary[List[Int]] = Arbitrary(genList)
  
}
