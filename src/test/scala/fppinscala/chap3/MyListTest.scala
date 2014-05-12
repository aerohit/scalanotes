package fppinscala.chap3

import org.specs2.mutable._
import org.specs2.execute._
import MyList._

class MyListTest extends Specification with PendingUntilFixed {
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

    // Ex 13
    "be able to implement foldLeft using foldRight" in {
      foldLeftUsingFR(MyList[Int](), 0)(_ + _) mustEqual 0
      foldLeftUsingFR(MyList(1), 0)(_ + _) mustEqual 1
      foldLeftUsingFR(MyList(1, 4, 8), 0)(_ + _) mustEqual 13
      foldLeftUsingFR(MyList[Int](), 1)(_ * _) mustEqual 1
      foldLeftUsingFR(MyList(7), 1)(_ * _) mustEqual 7
      foldLeftUsingFR(MyList(3, 5, 2), 1)(_ * _) mustEqual 30
    }

    "be able to implement foldRight using foldLeft" in {
      foldRightUsingFL(MyList[Int](), 0)(_ + _) mustEqual 0
      foldRightUsingFL(MyList(1), 0)(_ + _) mustEqual 1
      foldRightUsingFL(MyList(1, 4, 8), 0)(_ + _) mustEqual 13
      foldRightUsingFL(MyList[Int](), 1)(_ * _) mustEqual 1
      foldRightUsingFL(MyList(7), 1)(_ * _) mustEqual 7
      foldRightUsingFL(MyList(3, 5, 2), 1)(_ * _) mustEqual 30
    }

    // Ex 14
    "be able to append using foldLeft" in {
      appendFL(MyList(), MyList()) mustEqual MyList()
      appendFL(MyList(1), MyList()) mustEqual MyList(1)
      appendFL(MyList(), MyList(1)) mustEqual MyList(1)
      appendFL(MyList(7, 8, 9), MyList(3, 2, 1)) mustEqual MyList(7, 8, 9, 3, 2, 1)
    }.pendingUntilFixed

    "be able to append using foldRight" in {
      appendFR(MyList(), MyList()) mustEqual MyList()
      appendFR(MyList(1), MyList()) mustEqual MyList(1)
      appendFR(MyList(), MyList(1)) mustEqual MyList(1)
      appendFR(MyList(7, 8, 9), MyList(3, 2, 1)) mustEqual MyList(7, 8, 9, 3, 2, 1)
    }

    // Ex 15
    "be able to concatenate a list of lists" in {
      concatenate(MyList()) mustEqual MyList()
      concatenate(MyList(MyList(1))) mustEqual MyList(1)
      concatenate(MyList(MyList(9, 3, 5), MyList(1, 2))) mustEqual MyList(9, 3, 5, 1, 2)
    }

    // Ex 16
    "be able to add 1 to each element of a list of integers" in {
      mapS(MyList[Int]())(_ + 1) mustEqual MyList()
      mapS(MyList(1))(_ + 1) mustEqual MyList(2)
      mapS(MyList(3, 5, 7))(_ + 1) mustEqual MyList(4, 6, 8)
    }

    // Ex 17
    "be able to convert a list of doubles in to a list of strings" in {
      mapS(MyList[Double]())(_.toString) mustEqual MyList()
      mapS(MyList(2.0))(_.toString) mustEqual MyList("2.0")
      mapS(MyList(3.0, 5.0, 7.0))(_.toString) mustEqual MyList("3.0", "5.0", "7.0")
    }

    // Ex 19
    "be able to filter out elements based on a predicate" in {
      filter(MyList[Int]())(_ % 2 != 0) mustEqual MyList()
      filter(MyList(1, 2, 3, 4))(_ % 2 == 0) mustEqual MyList(2, 4)
    }

    // Ex 20
    "be able to flatMap an input list" in {
      flatMap(MyList[Int]())(x => MyList(x*x, 2*x)) mustEqual MyList()
      flatMap(MyList(1, 2, 3, 4))(x => MyList(x*x, 2*x)) mustEqual MyList(1, 2, 4, 4, 9, 6, 16, 8)
    }

    // Ex 21
    "be able to implement filter using flatMap" in {
      filterUsingFM(MyList[Int]())(_ % 2 != 0) mustEqual MyList()
      filterUsingFM(MyList(1, 2, 3, 4))(_ % 2 == 0) mustEqual MyList(2, 4)
    }

    // Ex 22
    "be able to sum corresponding elements of two lists" in {
      sumCorrespondingElements(MyList(1, 2, 3), MyList(4, 5, 6)) mustEqual(MyList(5, 7, 9))
    }

    // Ex 23
    "be able to combine two lists" in {
      combine(MyList(), MyList(4, 5, 6))((x: Int, y: Int) => x + y) mustEqual(MyList())
      combine(MyList(1, 2, 3), MyList())((x, y) => x + y) mustEqual(MyList())
      combine(MyList(1, 2, 3), MyList(4, 5, 6))((x, y) => x + y) mustEqual(MyList(5, 7, 9))
    }

    "be able to take some elements from a list" in {
      take(MyNil)(4) mustEqual MyNil
      take(MyList(1, 2, 3))(2) mustEqual MyList(1, 2)
      take(MyList(1, 2, 3))(4) mustEqual MyList(1, 2, 3)
    }

    "be able to take some elements while a condition is true" in {
      takeWhile(MyList[Int]())(_ <= 2) mustEqual MyNil
      takeWhile(MyList(1, 2, 3))(_ <= 2) mustEqual MyList(1, 2)
    }
  }
}

