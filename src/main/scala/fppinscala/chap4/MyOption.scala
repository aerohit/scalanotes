package fppinscala.chap4

sealed trait MyOption[+A]

case class MySome[A](get: A) extends MyOption[A]

case object MyNone extends MyOption[Nothing]

object MyOption {
  def mean(xs: Seq[Double]): MyOption[Double] = {
    if (xs.isEmpty)
      MyNone
    else
      MySome(xs.sum / xs.size)
  }
}
