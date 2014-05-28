package fppinscala.chap4

import org.specs2.mutable.Specification
import org.specs2.execute.PendingUntilFixed
import MyOption._

class MyOptionTest extends Specification with PendingUntilFixed {
  "The mean function" should {
    "return None if the sequence is empty" in {
      mean(Seq[Double]()) mustEqual MyNone
    }

    "calculate mean if the sequence is not empty" in {
      mean(Seq(1, 2, 3)) mustEqual MySome(2)
    }
  }

  "The options api" should {
    def square(x: Int): Int = x * x
    def positive(x: Int): Boolean = x >= 0
    def negative(x: Int): Boolean = x < 0

    val none: MyOption[Int] = MyNone
    val some: MyOption[Int] = MySome(2)

    "map over a function" in {
      none.map(square) mustEqual MyNone
      some.map(square) mustEqual MySome(4)
    }

    "flatMap over a function" in {
      none.flatMap((x: Int) => MySome(square(x))) mustEqual MyNone
      some.flatMap((x: Int) => MySome(square(x))) mustEqual MySome(4)
    }

    "return with a provided default value when none" in {
      none.getOrElse[Int](3) mustEqual 3
      some.getOrElse(4) mustEqual 2
    }

    "return with a provided default option when none" in {
      none.orElse(MySome(4)) mustEqual MySome(4)
      some.orElse(MySome(4)) mustEqual some
    }

    "return option only if the predicate is true" in {
      none.filter(positive) mustEqual MyNone
      none.filter(negative) mustEqual MyNone
      some.filter(positive) mustEqual some
      some.filter(negative) mustEqual MyNone
    }

    // Ex 2
    "be able to calculate variance using flatMap" in {
      1 == 3
    }.pendingUntilFixed

    "lift a function" in {
      val liftedSquare = lift(square)
      liftedSquare(none) mustEqual MyNone
      liftedSquare(some) mustEqual MySome(4)
    }
  }
}
