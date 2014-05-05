package fppinscala.chap2

import org.specs2.mutable._

class GettingStartedSampleTest extends Specification {
  import GettingStartedSample._
  "The function" should {
    "calculate factorial non-tail-recursively" in {
      factorialNonTailRec(0) mustEqual 1
      factorialNonTailRec(1) mustEqual 1
      factorialNonTailRec(2) mustEqual 2
      factorialNonTailRec(5) mustEqual 120
    }

    "calculate factorial tail-recursively" in {
      factorialTailRec(0) mustEqual 1
      factorialTailRec(1) mustEqual 1
      factorialTailRec(2) mustEqual 2
      factorialTailRec(5) mustEqual 120
    }
  }

  "A higher order function" should {
    "apply function to its arguments" in {
      applyFunc(3, (x: Int) => x * x) mustEqual 9
      applyFunc(3, (x) => x + 4) mustEqual 7
      applyFunc(3, x => x - 2) mustEqual 1
      applyFunc(3, _ * 8) mustEqual 24
    }
  }

  "A polymorphic binary search" should {
    "find a key inside an array of integers" in {
      val arr = Array(1,3,5,7,9)
      binarySearch(5, arr, (x: Int, y: Int) => x > y) mustEqual 2
      binarySearch(6, arr, (x: Int, y: Int) => x > y) must beLessThan(0)
    }
  }
}
