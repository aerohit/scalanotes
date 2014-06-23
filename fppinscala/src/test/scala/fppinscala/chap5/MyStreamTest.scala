package fppinscala.chap5

import org.specs2.execute.PendingUntilFixed
import org.specs2.mutable.Specification

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

  "can take first n elements of a stream" in {
    val stream = MyStream(1, 2, 3, 4)
    stream.take(2).toList mustEqual MyStream(1, 2).toList
  }

  "can take while a predicate is true" in {
    val stream = MyStream(1, 2, 3, 4, 1, 2)
    stream.takeWhile(n => n < 3).toList mustEqual MyStream(1, 2).toList
  }
}
