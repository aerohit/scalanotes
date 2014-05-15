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
}
