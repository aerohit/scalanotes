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
}
