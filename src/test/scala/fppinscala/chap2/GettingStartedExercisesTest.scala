import org.specs2.mutable._

class GettingStartedExercisesTest extends Specification {
  import GettingStartedExercises._
  "The function" should {
    "calculate fibonacci tail-recursively" in {
      fibonacci(1) mustEqual 0
      fibonacci(2) mustEqual 1
      fibonacci(3) mustEqual 1
      fibonacci(4) mustEqual 2
      fibonacci(5) mustEqual 3
      fibonacci(6) mustEqual 5
    }
  }
}
