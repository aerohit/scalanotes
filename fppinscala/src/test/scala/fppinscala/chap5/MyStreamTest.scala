package fppinscala.chap5

import org.specs2.mutable.Specification
import org.specs2.execute.PendingUntilFixed

import MyStream._

class MyStreamTest extends Specification with PendingUntilFixed {
  "can be converted to a list using basic recursion" in {
    val stream = MyStream(1, 2, 3)
    stream.toListRecursive mustEqual List(1, 2, 3)
  }

  "can be converted to a list using tail recursion" in {
    val stream = MyStream(1, 2, 3)
    stream.toListTailRecursive mustEqual List(1, 2, 3)
  }

  "can be converted to a list using fast recursion" in {
    val stream = MyStream(1, 2, 3)
    stream.toListTailRecursiveFast mustEqual List(1, 2, 3)
  }
}
