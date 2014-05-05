object GettingStartedExercises {
  // Ex 1
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, first: Int, second: Int): Int =
      if (n == 1) first
      else go(n - 1, second, first + second)

    go(n, 0, 1)
  }
}
