package neophytes

class RichList[T](value: List[T]) {
  def append(other: List[T]): List[T] = {
    value ::: other
  }
}

object ImplicitConversions extends App {
  // Implicit conversion to other types
  implicit def arrayToString[T](a: List[T]) = a.toString
  val arr = List(1, 2, 3)
  val str: String = arr     // this would be illegal without the above implicit conversion
  println(str)

  // Pimping the libraries
  implicit def enrichList[T](a: List[T]): RichList[T] = new RichList[T](a)
  val x = List(1, 2)
  val y = List(3, 4)
  // append isn't a method on the List class,
  // but 'x' gets implicitly converted to a RichList
  println(x append y)       
}


// References
// http://www.artima.com/weblogs/viewpost.jsp?thread=179766
