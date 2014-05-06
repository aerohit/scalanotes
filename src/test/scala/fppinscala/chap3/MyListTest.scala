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

    "be able to be tailed" in {
      tail(MyList()) must throwA[RuntimeException]
      tail(MyList(1)) mustEqual MyList()
      tail(MyList(7, 1, 2, 4)) mustEqual MyList(1, 2, 4)
    }

    "should support setHead" in {
      setHead(MyList(), 5) must throwA[RuntimeException]
      setHead(MyList(1), 3) mustEqual MyList(3)
      setHead(MyList(1, 2, 4, 6), 4) mustEqual MyList(4, 2, 4, 6)
    }
  }
}

