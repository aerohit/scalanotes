object GettingStartedSample {
  def factorialNonTailRec(n: Int): Int =
    if (n == 0) 1 
    else n * factorialNonTailRec(n - 1)

  def factorialTailRec(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n == 0) acc 
      else go(n - 1, n * acc)
    go(n, 1)
  }

  def applyFunc(x: Int, f: Int => Int) = f(x)
}
