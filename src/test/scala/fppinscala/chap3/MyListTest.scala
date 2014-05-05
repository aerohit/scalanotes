package fppinscala.chap3

import org.specs2.mutable._
import MyList._

class MyListTest extends Specification {
  "The list" should {
    "calculate sum of integers" in {
      sum(MyList()) mustEqual 0
      sum(MyList(1, 2, 3)) mustEqual 6
    }

    "calculate product of integers" in {
      product(MyList()) mustEqual 1
      product(MyList(1, 2, 3)) mustEqual 6
    }
  }
}

