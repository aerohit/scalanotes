package fppinscala.chap3

import org.specs2.mutable._
import MyList._

class MyListTest extends Specification {
  "The list api" should {
    "calculate sum of integers" in {
      sum(MyList()) mustEqual 0
      sum(MyList(1, 2, 3)) mustEqual 6
    }

    "calculate product of integers" in {
      product(MyList()) mustEqual 1
      product(MyList(1, 2, 3)) mustEqual 6
    }

    // Ex 2
    "be able to be tail a list" in {
      tail(MyList()) must throwA[RuntimeException]
      tail(MyList(1)) mustEqual MyList()
      tail(MyList(7, 1, 2, 4)) mustEqual MyList(1, 2, 4)
    }

    // Ex 3
    "allow to setHead" in {
      setHead(MyList(), 5) must throwA[RuntimeException]
      setHead(MyList(1), 3) mustEqual MyList(3)
      setHead(MyList(1, 2, 4, 6), 4) mustEqual MyList(4, 2, 4, 6)
    }

    // Ex 4
    "allow to drop elements" in {
      drop(MyList(), 1) must throwA[RuntimeException]
      drop(MyList(1, 2, 4, 6), 6) must throwA[RuntimeException]
      drop(MyList(), 0) mustEqual MyList()
      drop(MyList(1, 2, 4, 3), 3) mustEqual MyList(3)
      drop(MyList(1, 2, 4, 6), 4) mustEqual MyList()
    }

    // Ex 5
    "allow to drop elements while a predicate is true" in {
      dropWhile(MyList(), (x: Int) => x > 4) mustEqual MyList()
      dropWhile(MyList(8, 5, 1, 2, 4, 6, 9), (x: Int) => x > 4) mustEqual MyList(1, 2, 4, 6, 9)
    }

    "allow to drop elements when a predicate without type specification is provided" in {
      dropWhileCurried(MyList[Int]())(x => x > 4) mustEqual MyList()
      dropWhileCurried(MyList(8, 5, 1, 2, 4, 6, 9))(x => x > 4) mustEqual MyList(1, 2, 4, 6, 9)
    }

    "allow to append another list" in {
      append(MyList(), MyList()) mustEqual MyList()
      append(MyList(1), MyList()) mustEqual MyList(1)
      append(MyList(), MyList(1)) mustEqual MyList(1)
      append(MyList(7, 8, 9), MyList(3, 2, 1)) mustEqual MyList(7, 8, 9, 3, 2, 1)
    }

    "return init of a list" in {
      init(MyList()) must throwA[RuntimeException]
      init(MyList(1)) mustEqual MyList()
      init(MyList(1, 3, 5, 8)) mustEqual MyList(1, 3, 5)
    }

    "foldRight non-tail recursively" in {
      foldRightNTR(MyList[Int](), 0)(_ + _) mustEqual 0
      foldRightNTR(MyList(1), 0)(_ + _) mustEqual 1
      foldRightNTR(MyList(1, 4, 8), 0)(_ + _) mustEqual 13
      foldRightNTR(MyList[Int](), 1)(_ * _) mustEqual 1
      foldRightNTR(MyList(7), 1)(_ * _) mustEqual 7
      foldRightNTR(MyList(3, 5, 2), 1)(_ * _) mustEqual 30
    }

    "foldRight tail recursively" in {
      foldRightTR(MyList[Int](), 0)(_ + _) mustEqual 0
      foldRightTR(MyList(1), 0)(_ + _) mustEqual 1
      foldRightTR(MyList(1, 4, 8), 0)(_ + _) mustEqual 13
      foldRightTR(MyList[Int](), 1)(_ * _) mustEqual 1
      foldRightTR(MyList(7), 1)(_ * _) mustEqual 7
      foldRightTR(MyList(3, 5, 2), 1)(_ * _) mustEqual 30
    }

    "foldRight non-tail recursively with MyNil and MyCons(...)" in {
      foldRightNTR(MyList(3, 5, 2), MyList[Int]())(MyCons(_, _)) mustEqual MyList(3, 5, 2)
    }

    "compute sum of integers using foldRight" in {
      sumFR(MyList()) mustEqual 0
      sumFR(MyList(1, 2, 3)) mustEqual 6
    }

    "calculate product of integers using foldRight" in {
      productFR(MyList()) mustEqual 1
      productFR(MyList(1, 2, 3)) mustEqual 6
    }

    // Ex 9
    "compute length using foldRight" in {
      lengthFR(MyList[Int]()) mustEqual 0
      lengthFR(MyList(1)) mustEqual 1
      lengthFR(MyList(0, 0, 0)) mustEqual 3
    }

    // Ex 10
    "foldLeft tail recursively" in {
      foldLeftTR(MyList[Int](), 0)(_ + _) mustEqual 0
      foldLeftTR(MyList(1), 0)(_ + _) mustEqual 1
      foldLeftTR(MyList(1, 4, 8), 0)(_ + _) mustEqual 13
      foldLeftTR(MyList[Int](), 1)(_ * _) mustEqual 1
      foldLeftTR(MyList(7), 1)(_ * _) mustEqual 7
      foldLeftTR(MyList(3, 5, 2), 1)(_ * _) mustEqual 30
    }

    "foldLeft non-tail recursively" in {
      foldLeftNTR(MyList[Int](), 0)(_ + _) mustEqual 0
      foldLeftNTR(MyList(1), 0)(_ + _) mustEqual 1
      foldLeftNTR(MyList(1, 4, 8), 0)(_ + _) mustEqual 13
      foldLeftNTR(MyList[Int](), 1)(_ * _) mustEqual 1
      foldLeftNTR(MyList(7), 1)(_ * _) mustEqual 7
      foldLeftNTR(MyList(3, 5, 2), 1)(_ * _) mustEqual 30
    }

    // Ex 11
    "compute length using foldLeft" in {
      lengthFL(MyList[Int]()) mustEqual 0
      lengthFL(MyList(1)) mustEqual 1
      lengthFL(MyList(0, 0, 0)) mustEqual 3
    }

    "compute sum of integers using foldLeft" in {
      sumFL(MyList()) mustEqual 0
      sumFL(MyList(1, 2, 3)) mustEqual 6
    }

    "calculate product of integers using foldLeft" in {
      productFL(MyList()) mustEqual 1
      productFL(MyList(1, 2, 3)) mustEqual 6
    }

    // Ex 12
    "be able to reverse using foldLeft" in {
      reverseFL(MyList()) mustEqual MyList()
      reverseFL(MyList(1, 2, 3)) mustEqual MyList(3, 2, 1)
    }

    "be able to reverse using foldRight" in {
      reverseFR(MyList()) mustEqual MyList()
      reverseFR(MyList(1, 2, 3)) mustEqual MyList(3, 2, 1)
    }
  }
}

