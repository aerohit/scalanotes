package fppinscala.chap4

sealed trait MyEither[+E, +A]
case class MyLeft[+E](value: E) extends MyEither[E, Nothing]
case class MyRight[+A](value: A) extends MyEither[Nothing, A]

object MyEither {
  def mean(xs: Seq[Double]): MyEither[String, Double] = {
    if(xs.isEmpty) MyLeft("error: mean of empty")
    else MyRight(xs.sum/xs.size)
  }

  def safeDiv(x: Int, y: Int): MyEither[String, Int] =
    try MyRight(x/y)
    catch { case e: Exception => MyLeft(e.toString) }

  def myTry[A](a: => A): MyEither[String, A] =
    try MyRight(a)
    catch { case e: Exception => MyLeft(e.toString) }
}
