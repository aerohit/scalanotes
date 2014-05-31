package fppinscala.chap2

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

  def binarySearch[A](key: A, arr: Array[A], gt: (A, A) => Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = arr(mid2)
        if (!gt(key, a) && !gt(a, key)) mid2
        else if (gt(key, a)) go(mid2 + 1, mid2, high)
        else go(low, mid2, mid2 - 1)
      }
    }
    go(0, arr.length / 2, arr.length - 1)
  }
}
