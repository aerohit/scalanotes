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

    // Ex 2
    "be able to be tailed" in {
      tail(MyList()) must throwA[RuntimeException]
      tail(MyList(1)) mustEqual MyList()
      tail(MyList(7, 1, 2, 4)) mustEqual MyList(1, 2, 4)
    }

    // Ex 3
    "support setHead" in {
      setHead(MyList(), 5) must throwA[RuntimeException]
      setHead(MyList(1), 3) mustEqual MyList(3)
      setHead(MyList(1, 2, 4, 6), 4) mustEqual MyList(4, 2, 4, 6)
    }

    // Ex 4
    "drop elements" in {
      drop(MyList(), 1) must throwA[RuntimeException]
      drop(MyList(1, 2, 4, 6), 6) must throwA[RuntimeException]
      drop(MyList(), 0) mustEqual MyList()
      drop(MyList(1, 2, 4, 3), 3) mustEqual MyList(3)
      drop(MyList(1, 2, 4, 6), 4) mustEqual MyList()
    }

    // Ex 5
    "drop while predicate is true" in {
      dropWhile(MyList(), (x: Int) => x > 4) mustEqual MyList()
      dropWhile(MyList(8, 5, 1, 2, 4, 6, 9), (x: Int) => x > 4) mustEqual MyList(1, 2, 4, 6, 9)
    }

    "support drop while predicates without type specification with a curried definition" in {
      dropWhileCurried(MyList[Int]())(x => x > 4) mustEqual MyList()
      dropWhileCurried(MyList(8, 5, 1, 2, 4, 6, 9))(x => x > 4) mustEqual MyList(1, 2, 4, 6, 9)
    }

    "append another list" in {
      append(MyList(), MyList()) mustEqual MyList()
      append(MyList(1), MyList()) mustEqual MyList(1)
      append(MyList(), MyList(1)) mustEqual MyList(1)
      append(MyList(7, 8, 9), MyList(3, 2, 1)) mustEqual MyList(7, 8, 9, 3, 2, 1)
    }

    "return init" in {
      init(MyList()) must throwA[RuntimeException]
      init(MyList(1)) mustEqual MyList()
      init(MyList(1, 3, 5, 8)) mustEqual MyList(1, 3, 5)
    }

    "foldRightNTR" in {
      foldRightNTR(MyList[Int](), 0)(_ + _) mustEqual 0
      foldRightNTR(MyList(1), 0)(_ + _) mustEqual 1
      foldRightNTR(MyList(1, 4, 8), 0)(_ + _) mustEqual 13
      foldRightNTR(MyList[Int](), 1)(_ * _) mustEqual 1
      foldRightNTR(MyList(7), 1)(_ * _) mustEqual 7
      foldRightNTR(MyList(3, 5, 2), 1)(_ * _) mustEqual 30
    }

    "foldRightNTR with MyNil and MyCons(...)" in {
      foldRightNTR(MyList(3, 5, 2), MyList[Int]())(MyCons(_, _)) mustEqual MyList(3, 5, 2)
    }

    "compute sum of integerst using foldRightNTR" in {
      sumFR(MyList()) mustEqual 0
      sumFR(MyList(1, 2, 3)) mustEqual 6
    }

    "calculate product of integers using foldRightNTR" in {
      productFR(MyList()) mustEqual 1
      productFR(MyList(1, 2, 3)) mustEqual 6
    }

    // Ex 9
    "compute length using foldRightNTR" in {
      lengthFR(MyList[Int]()) mustEqual 0
      lengthFR(MyList(1)) mustEqual 1
      lengthFR(MyList(0, 0, 0)) mustEqual 3
    }

    // Ex 10
    "foldLeftTR" in {
      foldLeftTR(MyList[Int](), 0)(_ + _) mustEqual 0
      foldLeftTR(MyList(1), 0)(_ + _) mustEqual 1
      foldLeftTR(MyList(1, 4, 8), 0)(_ + _) mustEqual 13
      foldLeftTR(MyList[Int](), 1)(_ * _) mustEqual 1
      foldLeftTR(MyList(7), 1)(_ * _) mustEqual 7
      foldLeftTR(MyList(3, 5, 2), 1)(_ * _) mustEqual 30
    }

    // Ex 11
    "compute length using foldLeftTR" in {
      lengthFL(MyList[Int]()) mustEqual 0
      lengthFL(MyList(1)) mustEqual 1
      lengthFL(MyList(0, 0, 0)) mustEqual 3
    }

    "compute sum of integerst using foldRightNTR" in {
      sumFL(MyList()) mustEqual 0
      sumFL(MyList(1, 2, 3)) mustEqual 6
    }

    "calculate product of integers using foldRightNTR" in {
      productFL(MyList()) mustEqual 1
      productFL(MyList(1, 2, 3)) mustEqual 6
    }

    // Ex 12
    "be able to be reversed using foldLeftTR" in {
      reverseFL(MyList()) mustEqual MyList()
      reverseFL(MyList(1, 2, 3)) mustEqual MyList(3, 2, 1)
    }

    "be able to be reversed using foldRightNTR" in {
      reverseFR(MyList()) mustEqual MyList()
      reverseFR(MyList(1, 2, 3)) mustEqual MyList(3, 2, 1)
    }
  }
}

