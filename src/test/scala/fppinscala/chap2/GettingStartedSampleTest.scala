import org.specs2.mutable._

class GettingStartedSampleTest extends Specification {
  import GettingStartedSample._
  "The function" should {
    "calculate factorial non-tail-recursively" in {
      factorialNonTailRec(0) mustEqual 1
      factorialNonTailRec(1) mustEqual 1
      factorialNonTailRec(2) mustEqual 2
      factorialNonTailRec(5) mustEqual 120
    }

    "calculate factorial tail-recursively" in {
      factorialTailRec(0) mustEqual 1
      factorialTailRec(1) mustEqual 1
      factorialTailRec(2) mustEqual 2
      factorialTailRec(5) mustEqual 120
    }
  }
}
