import org.specs2.mutable._
import org.specs2.execute._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

class GettingStartedExercisesTest extends Specification with PendingUntilFixed {
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
      val expectedFunc =  (x: Int) => x + 1
      // why can't we do the following?
      subject mustEqual expectedFunc
    }.pendingUntilFixed

    // Ex 4
    "curry a function of N arguments into a function of 1 argument" in {
      val subject = curry((x: Int, y: Int) => x * y)
      val expectedFunc =  (x: Int) => (y: Int) => x * y
      // why can't we do the following?
      subject mustEqual expectedFunc
    }.pendingUntilFixed

    // Ex 5
    "uncurry" in {
      val subject = uncurry((x: Int) => (y: Int) => x - y)
      val expectedFunc = (x: Int, y: Int) => x - y
      // why can't we do the following?
      subject mustEqual expectedFunc
    }.pendingUntilFixed

    // Ex 6
    "compose" in {
      val subject = compose((x: Int) => 2 * x, (x: Int) => x + 2)
      val expectedFunc = (x: Int) => 2 * x + 4
      // why can't we do the following?
      subject mustEqual expectedFunc
    }.pendingUntilFixed
  }
}

object GettingStartedExercisesSpecification extends Properties("functions") {
  import GettingStartedExercises._
  // Ex 3
  property("partial application") = forAll { (a: Int) =>
    val subject = partial1(1, (x: Int, y: Int) => x + y)
    val expectedFunc =  (x: Int) => x + 1
    subject(a) == expectedFunc(a)
  }

  // Ex 4
  property("currying") = forAll { (a: Int, b: Int) =>
    val subject = curry((x: Int, y: Int) => x * y)
    val expectedFunc =  (x: Int) => (y: Int) => x * y
    subject(a)(b) == expectedFunc(a)(b)
  }

  // Ex 5
  property("uncurrying") = forAll { (a: Int, b: Int) =>
    val subject = uncurry((x: Int) => (y: Int) => x - y)
    val expectedFunc = (x: Int, y: Int) => x - y
    subject(a, b) == expectedFunc(a, b)
  }

  // Ex 6
  property("composition") = forAll { (a: Int) =>
    val subject = compose((x: Int) => 2 * x, (x: Int) => x + 2)
    val expectedFunc = (x: Int) => 2 * x + 4
    subject(a) == expectedFunc(a)
  }
}
