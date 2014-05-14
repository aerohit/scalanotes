package fppinscala.chap3

import org.specs2.mutable._
import org.specs2.execute._
import MyTree._

class MyTreeTest extends Specification with PendingUntilFixed {
  "The tree api" should {
    val l1 = MyLeaf[Int](1)
    val l2 = MyLeaf[Int](2)
    val l3 = MyLeaf[Int](3)
    val l4 = MyLeaf[Int](4)
    val b1 = MyBranch(l1, l2)
    val b2 = MyBranch(l3, l4)
    val r = MyBranch(b1, b2)

    val sl1 = MyLeaf[Int](1)
    val sl2 = MyLeaf[Int](4)
    val sl3 = MyLeaf[Int](9)
    val sl4 = MyLeaf[Int](16)
    val sb1 = MyBranch(sl1, sl2)
    val sb2 = MyBranch(sl3, sl4)
    val sr = MyBranch(sb1, sb2)

    // Ex 25
    "count the number of nodes in a tree" in {
      treeSize(l1) mustEqual 1
      treeSize(b1) mustEqual 3
      treeSize(r) mustEqual 7
    }

    // Ex 26
    "find the maximum element in a tree" in {
      implicit def compInt(x: Int, y: Int): Int = x max y
      maximum(l1) mustEqual 1
      maximum(b1) mustEqual 2
      maximum(r) mustEqual 4
    }

    // Ex 27
    "find the depth of a tree" in {
      depth(l1) mustEqual 0
      depth(b1) mustEqual 1
      depth(r) mustEqual 2
    }

    // Ex 28
    "map a tree to another" in {
      treeMap(l1)(x => x * x) mustEqual sl1
      treeMap(b1)(x => x * x) mustEqual sb1
      treeMap(r)(x => x * x) mustEqual sr
    }

    // Ex 29
    "should be able to implement size, maximum, depth, treeMap using fold" in {
      treeSizeF(l1) mustEqual 1
      treeSizeF(b1) mustEqual 3
      treeSizeF(r) mustEqual 7

      implicit def compInt(x: Int, y: Int): Int = x max y
      maximumF(l1) mustEqual 1
      maximumF(b1) mustEqual 2
      maximumF(r) mustEqual 4

      depthF(l1) mustEqual 0
      depthF(b1) mustEqual 1
      depthF(r) mustEqual 2

      treeMapF(l1)(x => x * x) mustEqual sl1
      treeMapF(b1)(x => x * x) mustEqual sb1
      treeMapF(r)(x => x * x) mustEqual sr
    }
  }
}
