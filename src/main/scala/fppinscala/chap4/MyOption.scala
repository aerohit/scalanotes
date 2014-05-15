package fppinscala.chap4

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(v) => MySome(f(v))
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(v) => f(v)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case MyNone => default
    case MySome(v) => v
  }

  def orElse[B >: A](default: => MyOption[B]): MyOption[B] = this match {
    case MyNone => default
    case x => x
  }

  def filter(p: A => Boolean): MyOption[A] = this match {
    case MyNone => MyNone
    case x@MySome(v) =>
      if (p(v)) x
      else MyNone
  }
}

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
