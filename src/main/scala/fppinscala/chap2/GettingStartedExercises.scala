object GettingStartedExercises {
  // Ex 1
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, first: Int, second: Int): Int =
      if (n == 1) first
      else go(n - 1, second, first + second)

    go(n, 0, 1)
  }

  // Ex 2
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(curr: Int, prev: Int): Boolean = {
      if (curr >= as.length) true
      else if (gt(as(prev), as(curr))) go(curr + 1, curr)
      else false
    }
    go(1, 0)
  }

  // Ex 3
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    b => f(a, b)
  }

  // Ex 4
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  // Ex 5
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // Ex 6
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
