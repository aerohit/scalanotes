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
    def sum(x: Int, y: Int): Int = x + y

    val none: MyOption[Int] = MyNone
    val some: MyOption[Int] = MySome(2)

    "map over a function" in {
      none.map(square) mustEqual MyNone
      some.map(square) mustEqual MySome(4)
    }

    "flatMap over a function" in {
      none.flatMap((x: Int) => MySome(square(x))) mustEqual MyNone
      none.flatMap((x: Int) => MyNone) mustEqual MyNone
      some.flatMap((x: Int) => MySome(square(x))) mustEqual MySome(4)
      some.flatMap((x: Int) => MyNone) mustEqual MyNone
    }

    "implement flatMap using map" in {
      none.flatMapUsingMap((x: Int) => MySome(square(x))) mustEqual MyNone
      none.flatMapUsingMap((x: Int) => MyNone) mustEqual MyNone
      some.flatMapUsingMap((x: Int) => MySome(square(x))) mustEqual MySome(4)
      some.flatMapUsingMap((x: Int) => MyNone) mustEqual MyNone
    }

    "return with a provided default value when none" in {
      none.getOrElse[Int](3) mustEqual 3
      some.getOrElse(4) mustEqual 2
    }

    "return with a provided default option when none" in {
      none.orElse(MySome(4)) mustEqual MySome(4)
      none.orElse(MyNone) mustEqual MyNone
      some.orElse(MySome(4)) mustEqual some
      some.orElse(MyNone) mustEqual some
    }

    "implement orElse using map" in {
      none.orElseUningMap(MySome(4)) mustEqual MySome(4)
      none.orElseUningMap(MyNone) mustEqual MyNone
      some.orElseUningMap(MySome(4)) mustEqual some
      some.orElseUningMap(MyNone) mustEqual some
    }

    "return option only if the predicate is true" in {
      none.filter(positive) mustEqual MyNone
      none.filter(negative) mustEqual MyNone
      some.filter(positive) mustEqual some
      some.filter(negative) mustEqual MyNone
    }

    "implement filter using flatMap" in {
      none.filterUsingFlatMap(positive) mustEqual MyNone
      none.filterUsingFlatMap(negative) mustEqual MyNone
      some.filterUsingFlatMap(positive) mustEqual some
      some.filterUsingFlatMap(negative) mustEqual MyNone
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

    "be able to wrap an exception throwing task in to a Try" in {
      myTry("3".toInt) mustEqual MySome(3)
      myTry("".toInt) mustEqual MyNone
    }

    "be able to map2" in {
      map2(MyNone, MyNone)(sum) mustEqual MyNone
      map2(MyNone, MySome(2))(sum) mustEqual MyNone
      map2(MySome(2), MyNone)(sum) mustEqual MyNone
      map2(MySome(2), MySome(3))(sum) mustEqual MySome(5)
    }

    "implement map2 using flatMap" in {
      map2UsingFlatMap(MyNone, MyNone)(sum) mustEqual MyNone
      map2UsingFlatMap(MyNone, MySome(2))(sum) mustEqual MyNone
      map2UsingFlatMap(MySome(2), MyNone)(sum) mustEqual MyNone
      map2UsingFlatMap(MySome(2), MySome(3))(sum) mustEqual MySome(5)
    }

    "be able to calculate insurance rate quote" in {
      parseInsuranceRateQuote("", "") mustEqual MyNone
      parseInsuranceRateQuote("2", "") mustEqual MyNone
      parseInsuranceRateQuote("", "2") mustEqual MyNone
      parseInsuranceRateQuote("2", "7") mustEqual MySome(0.14)
    }
  }
}
