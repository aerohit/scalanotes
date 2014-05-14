package fppinscala.chap3

import org.specs2.mutable._
import org.specs2.execute._
import MyTree._

class MyTreeTest extends Specification with PendingUntilFixed {
  "The tree api" should {
    "calculate sum of integers" in {
      sum(MyNil) mustEqual 0
      sum(MyList(1, 2, 3)) mustEqual 6
    }
  }
}
