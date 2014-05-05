import org.specs2.mutable._

class GettingStartedExercisesTest extends Specification {
  import GettingStartedExercises._
  "The function" should {
    // Ex 1
    "calculate fibonacci tail-recursively" in {
      fibonacci(1) mustEqual 0
      fibonacci(2) mustEqual 1
      fibonacci(3) mustEqual 1
      fibonacci(4) mustEqual 2
      fibonacci(5) mustEqual 3
      fibonacci(6) mustEqual 5
    }

    // Ex 2
    "check if an array is sorted" in {
      isSorted[Int](Array(), (x,y) => x < y) mustEqual true
      isSorted[Int](Array(1, 3, 5), (x,y) => x < y) mustEqual true
      isSorted[Int](Array(1, 9, 5), (x,y) => x < y) mustEqual false
      isSorted[Int](Array(11, 9, 5),(x,y) => x > y) mustEqual true
    }

    // Ex 3
    "partially apply" in {
      val subject = partial1(1, (x: Int, y: Int) => x + y)
      val expectedFunc: Int => Int = { x: Int => x + 1 }
      subject(4) mustEqual expectedFunc(4)
      // why can't we do the following?
      subject mustEqual expectedFunc
    }

    // Ex 4
    "curry a function of N arguments into a function of 1 argument" in {
      val subject = curry((x: Int, y: Int) => x * y)
      val expectedFunc = { x: Int => {y: Int => x * y} }
      subject(3)(4) mustEqual expectedFunc(3)(4)
      // why can't we do the following?
      subject mustEqual expectedFunc
    }
  }
}
