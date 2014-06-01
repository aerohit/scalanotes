package fppinscala.chap4

import org.specs2.mutable.Specification
import org.specs2.execute.PendingUntilFixed

import MyEither._

class MyEitherTest extends Specification with PendingUntilFixed {
  "The mean function" should {
    "return error if the sequence is empty" in {
      mean(Seq[Double]()) mustEqual MyLeft("error: mean of empty")
    }

    "calculate mean if the sequence is not empty" in {
      mean(Seq(1, 2, 3)) mustEqual MyRight(2)
    }
  }

  "The safeDiv function" should {
    "return error when division by zero" in {
      safeDiv(2, 0) mustEqual MyLeft("java.lang.ArithmeticException: / by zero")
      safeDiv(4, 2) mustEqual MyRight(2)
    }
  }

  "The either's api" should {
    def square(x: Int): Int = x * x
    def sum(x: Int, y: Int): Int = x + y
    def squareE(x: Int): MyEither[String, Int] = MyRight(x * x)
    val l: MyEither[String, Int] = MyLeft("error")
    val r: MyEither[String, Int] = MyRight(2)

    "be able to wrap an exception throwing task in to a Try" in {
      myTry("3".toInt) mustEqual MyRight(3)
      myTry("".toInt) mustEqual MyLeft("java.lang.NumberFormatException: For input string: \"\"")
    }

    "be able to map on the right projection" in {
      l.mapR(square) mustEqual MyLeft("error")
      r.mapR(square) mustEqual MyRight(4)
    }

    "be able to flatMap on the right projection" in {
      l.flatMapR(squareE) mustEqual MyLeft("error")
      r.flatMapR(squareE) mustEqual MyRight(4)
    }

    "be able to orElse on the right projection" in {
      l.orElseR(MyRight(4)) mustEqual MyRight(4)
      r.orElseR(MyRight(4)) mustEqual MyRight(2)
    }

    "be able to map2 on the right projection" in {
      l.map2R(MyRight(5))(sum) mustEqual MyLeft("error")
      r.map2R(MyRight(5))(sum) mustEqual MyRight(7)
    }
  }
}
