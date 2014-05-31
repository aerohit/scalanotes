package fppinscala.chap3

import org.specs2.mutable._
import org.specs2.execute._
import MyList._

class MyListTest extends Specification with PendingUntilFixed {
  private val emptyInts = MyList[Int]()

  "The list api" should {
    "calculate sum of integers" in {
      sum(MyNil) mustEqual 0
      sum(MyList(1, 2, 3)) mustEqual 6
    }

    "calculate product of integers" in {
      product(MyNil) mustEqual 1
      product(MyList(1, 2, 3)) mustEqual 6
    }

    // Ex 2
    "be able to be tail a list" in {
      tail(MyNil) must throwA[RuntimeException]
      tail(MyList(1)) mustEqual MyNil
      tail(MyList(7, 1, 2, 4)) mustEqual MyList(1, 2, 4)
    }

    // Ex 3
    "allow to setHead" in {
      setHead(MyNil, 5) must throwA[RuntimeException]
      setHead(MyList(1), 3) mustEqual MyList(3)
      setHead(MyList(1, 2, 4, 6), 4) mustEqual MyList(4, 2, 4, 6)
    }

    // Ex 4
    "allow to drop elements" in {
      drop(MyNil, 1) must throwA[RuntimeException]
      drop(MyList(1, 2, 4, 6), 6) must throwA[RuntimeException]
      drop(MyNil, 0) mustEqual MyNil
      drop(MyList(1, 2, 4, 3), 3) mustEqual MyList(3)
      drop(MyList(1, 2, 4, 6), 4) mustEqual MyNil
    }

    // Ex 5
    "allow to drop elements while a predicate is true" in {
      dropWhile(MyNil, (x: Int) => x > 4) mustEqual MyNil
      dropWhile(MyList(8, 5, 1, 2, 4, 6, 9), (x: Int) => x > 4) mustEqual MyList(1, 2, 4, 6, 9)
    }

    "allow to drop elements when a predicate without type specification is provided" in {
      dropWhileCurried(emptyInts)(x => x > 4) mustEqual MyNil
      dropWhileCurried(MyList(8, 5, 1, 2, 4, 6, 9))(x => x > 4) mustEqual MyList(1, 2, 4, 6, 9)
    }

    "allow to append another list" in {
      append(MyNil, MyNil) mustEqual MyNil
      append(MyList(1), MyNil) mustEqual MyList(1)
      append(MyNil, MyList(1)) mustEqual MyList(1)
      append(MyList(7, 8, 9), MyList(3, 2, 1)) mustEqual MyList(7, 8, 9, 3, 2, 1)
    }

    "return init of a list" in {
      init(MyNil) must throwA[RuntimeException]
      init(MyList(1)) mustEqual MyNil
      init(MyList(1, 3, 5, 8)) mustEqual MyList(1, 3, 5)
    }

    "foldRight non-tail recursively" in {
      foldRightNTR(emptyInts, 0)(_ + _) mustEqual 0
      foldRightNTR(MyList(1), 0)(_ + _) mustEqual 1
      foldRightNTR(MyList(1, 4, 8), 0)(_ + _) mustEqual 13
      foldRightNTR(emptyInts, 1)(_ * _) mustEqual 1
      foldRightNTR(MyList(7), 1)(_ * _) mustEqual 7
      foldRightNTR(MyList(3, 5, 2), 1)(_ * _) mustEqual 30
    }

    "foldRight tail recursively" in {
      foldRightTR(emptyInts, 0)(_ + _) mustEqual 0
      foldRightTR(MyList(1), 0)(_ + _) mustEqual 1
      foldRightTR(MyList(1, 4, 8), 0)(_ + _) mustEqual 13
      foldRightTR(emptyInts, 1)(_ * _) mustEqual 1
      foldRightTR(MyList(7), 1)(_ * _) mustEqual 7
      foldRightTR(MyList(3, 5, 2), 1)(_ * _) mustEqual 30
    }

    "foldRight non-tail recursively with MyNil and MyCons(...)" in {
      foldRightNTR(MyList(3, 5, 2), emptyInts)(MyCons(_, _)) mustEqual MyList(3, 5, 2)
    }

    "compute sum of integers using foldRight" in {
      sumFR(MyNil) mustEqual 0
      sumFR(MyList(1, 2, 3)) mustEqual 6
    }

    "calculate product of integers using foldRight" in {
      productFR(MyNil) mustEqual 1
      productFR(MyList(1, 2, 3)) mustEqual 6
    }

    // Ex 9
    "compute length using foldRight" in {
      lengthFR(emptyInts) mustEqual 0
      lengthFR(MyList(1)) mustEqual 1
      lengthFR(MyList(0, 0, 0)) mustEqual 3
    }

    // Ex 10
    "foldLeft tail recursively" in {
      foldLeftTR(emptyInts, 0)(_ + _) mustEqual 0
      foldLeftTR(MyList(1), 0)(_ + _) mustEqual 1
      foldLeftTR(MyList(1, 4, 8), 0)(_ + _) mustEqual 13
      foldLeftTR(emptyInts, 1)(_ * _) mustEqual 1
      foldLeftTR(MyList(7), 1)(_ * _) mustEqual 7
      foldLeftTR(MyList(3, 5, 2), 1)(_ * _) mustEqual 30
    }

    "foldLeft non-tail recursively" in {
      foldLeftNTR(emptyInts, 0)(_ + _) mustEqual 0
      foldLeftNTR(MyList(1), 0)(_ + _) mustEqual 1
      foldLeftNTR(MyList(1, 4, 8), 0)(_ + _) mustEqual 13
      foldLeftNTR(emptyInts, 1)(_ * _) mustEqual 1
      foldLeftNTR(MyList(7), 1)(_ * _) mustEqual 7
      foldLeftNTR(MyList(3, 5, 2), 1)(_ * _) mustEqual 30
    }

    // Ex 11
    "compute length using foldLeft" in {
      lengthFL(emptyInts) mustEqual 0
      lengthFL(MyList(1)) mustEqual 1
      lengthFL(MyList(0, 0, 0)) mustEqual 3
    }

    "compute sum of integers using foldLeft" in {
      sumFL(MyNil) mustEqual 0
      sumFL(MyList(1, 2, 3)) mustEqual 6
    }

    "calculate product of integers using foldLeft" in {
      productFL(MyNil) mustEqual 1
      productFL(MyList(1, 2, 3)) mustEqual 6
    }

    // Ex 12
    "be able to reverse using foldLeft" in {
      reverseFL(MyNil) mustEqual MyNil
      reverseFL(MyList(1, 2, 3)) mustEqual MyList(3, 2, 1)
    }

    "be able to reverse using foldRight" in {
      reverseFR(MyNil) mustEqual MyNil
      reverseFR(MyList(1, 2, 3)) mustEqual MyList(3, 2, 1)
    }

    // Ex 13
    "be able to implement foldLeft using foldRight" in {
      foldLeftUsingFR(emptyInts, 0)(_ + _) mustEqual 0
      foldLeftUsingFR(MyList(1), 0)(_ + _) mustEqual 1
      foldLeftUsingFR(MyList(1, 4, 8), 0)(_ + _) mustEqual 13
      foldLeftUsingFR(emptyInts, 1)(_ * _) mustEqual 1
      foldLeftUsingFR(MyList(7), 1)(_ * _) mustEqual 7
      foldLeftUsingFR(MyList(3, 5, 2), 1)(_ * _) mustEqual 30
    }

    "be able to implement foldRight using foldLeft" in {
      foldRightUsingFL(emptyInts, 0)(_ + _) mustEqual 0
      foldRightUsingFL(MyList(1), 0)(_ + _) mustEqual 1
      foldRightUsingFL(MyList(1, 4, 8), 0)(_ + _) mustEqual 13
      foldRightUsingFL(emptyInts, 1)(_ * _) mustEqual 1
      foldRightUsingFL(MyList(7), 1)(_ * _) mustEqual 7
      foldRightUsingFL(MyList(3, 5, 2), 1)(_ * _) mustEqual 30
    }

    // Ex 14
    "be able to append using foldLeft" in {
      appendFL(MyNil, MyNil) mustEqual MyNil
      appendFL(MyList(1), MyNil) mustEqual MyList(1)
      appendFL(MyNil, MyList(1)) mustEqual MyList(1)
      appendFL(MyList(7, 8, 9), MyList(3, 2, 1)) mustEqual MyList(7, 8, 9, 3, 2, 1)
    }.pendingUntilFixed

    "be able to append using foldRight" in {
      appendFR(MyNil, MyNil) mustEqual MyNil
      appendFR(MyList(1), MyNil) mustEqual MyList(1)
      appendFR(MyNil, MyList(1)) mustEqual MyList(1)
      appendFR(MyList(7, 8, 9), MyList(3, 2, 1)) mustEqual MyList(7, 8, 9, 3, 2, 1)
    }

    // Ex 15
    "be able to concatenate a list of lists" in {
      concatenate(MyNil) mustEqual MyNil
      concatenate(MyList(MyList(1))) mustEqual MyList(1)
      concatenate(MyList(MyList(9, 3, 5), MyList(1, 2))) mustEqual MyList(9, 3, 5, 1, 2)
    }

    // Ex 16
    "be able to add 1 to each element of a list of integers" in {
      mapS(emptyInts)(_ + 1) mustEqual MyNil
      mapS(MyList(1))(_ + 1) mustEqual MyList(2)
      mapS(MyList(3, 5, 7))(_ + 1) mustEqual MyList(4, 6, 8)
    }

    // Ex 17
    "be able to convert a list of doubles in to a list of strings" in {
      mapS(MyList[Double]())(_.toString) mustEqual MyNil
      mapS(MyList(2.0))(_.toString) mustEqual MyList("2.0")
      mapS(MyList(3.0, 5.0, 7.0))(_.toString) mustEqual MyList("3.0", "5.0", "7.0")
    }

    // Ex 19
    "be able to filter out elements based on a predicate" in {
      filter(emptyInts)(_ % 2 != 0) mustEqual MyNil
      filter(MyList(1, 2, 3, 4))(_ % 2 == 0) mustEqual MyList(2, 4)
    }

    // Ex 20
    "be able to flatMap an input list" in {
      flatMap(emptyInts)(x => MyList(x*x, 2*x)) mustEqual MyNil
      flatMap(MyList(1, 2, 3, 4))(x => MyList(x*x, 2*x)) mustEqual MyList(1, 2, 4, 4, 9, 6, 16, 8)
    }

    // Ex 21
    "be able to implement filter using flatMap" in {
      filterUsingFM(emptyInts)(_ % 2 != 0) mustEqual MyNil
      filterUsingFM(MyList(1, 2, 3, 4))(_ % 2 == 0) mustEqual MyList(2, 4)
    }

    // Ex 22
    "be able to sum corresponding elements of two lists" in {
      sumCorrespondingElements(MyList(1, 2, 3), MyList(4, 5, 6)) mustEqual(MyList(5, 7, 9))
    }

    // Ex 23
    "be able to combine two lists" in {
      combine(MyNil, MyList(4, 5, 6))((x: Int, y: Int) => x + y) mustEqual(MyNil)
      combine(MyList(1, 2, 3), MyNil)((x, y) => x + y) mustEqual(MyNil)
      combine(MyList(1, 2, 3), MyList(4, 5, 6))((x, y) => x + y) mustEqual(MyList(5, 7, 9))
    }

    "be able to take some elements from a list" in {
      take(MyNil)(4) mustEqual MyNil
      take(MyList(1, 2, 3))(2) mustEqual MyList(1, 2)
      take(MyList(1, 2, 3))(4) mustEqual MyList(1, 2, 3)
    }

    "be able to take some elements while a condition is true" in {
      takeWhile(emptyInts)(_ <= 2) mustEqual MyNil
      takeWhile(MyList(1, 2, 3))(_ <= 2) mustEqual MyList(1, 2)
    }

    "be able to verify a predicate forall elements" in {
      myForAll(emptyInts)(_ > 0) must beTrue
      myForAll(emptyInts)(_ < 0) must beTrue
      myForAll(MyList(1, 2, 3, 4))(_ > 0) must beTrue
      myForAll(MyList(1, 2, 3, 4))(_ > 2) must beFalse
    }

    "be able to do an existance test" in {
      exists(emptyInts)(_ > 0) must beFalse
      exists(emptyInts)(_ < 0) must beFalse
      exists(MyList(1, 2, 3, 4))(_ > 0) must beTrue
      exists(MyList(-1, -2, -3, -4))(_ > 0) must beFalse
    }

    "be able to scan left" in {
      scanLeft(emptyInts, 0)(_ + _) mustEqual MyList(0)
      scanLeft(MyList(1), 0)(_ + _) mustEqual MyList(0, 1)
      scanLeft(MyList(1, 2, 3), 0)(_ + _) mustEqual MyList(0, 1, 3, 6)
      scanLeft(emptyInts, 1)(_ * _) mustEqual MyList(1)
      scanLeft(MyList(1), 1)(_ * _) mustEqual MyList(1, 1)
      scanLeft(MyList(1, 2, 3), 1)(_ * _) mustEqual MyList(1, 1, 2, 6)
    }

    "be able to scan right" in {
      scanRight(emptyInts, 0)(_ + _) mustEqual MyList(0)
      scanRight(MyList(1), 0)(_ + _) mustEqual MyList(1, 0)
      scanRight(MyList(1, 2, 3), 0)(_ + _) mustEqual MyList(6, 5, 3, 0)
      scanRight(emptyInts, 1)(_ * _) mustEqual MyList(1)
      scanRight(MyList(1), 1)(_ * _) mustEqual MyList(1, 1)
      scanRight(MyList(1, 2, 3), 1)(_ * _) mustEqual MyList(6, 6, 3, 1)
    }.pendingUntilFixed

    // Ex 24
    "be able to check if a list is a sublist of another" in {
      hasSubsequence(emptyInts, emptyInts) must beTrue
      hasSubsequence(emptyInts, MyList(1)) must beFalse
      hasSubsequence(MyList(1, 2, 3, 4), MyList(1, 2)) must beTrue
      hasSubsequence(MyList(1, 2, 3, 4), MyList(3, 4)) must beTrue
      hasSubsequence(MyList(1, 2, 3, 4), MyList(4, 3)) must beFalse
      hasSubsequence(MyList(1, 2, 3), MyList(1, 2, 3, 4)) must beFalse
    }.pendingUntilFixed
  }
}

