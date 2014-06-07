package fppinscala.chap5

import fppinscala.chap4._

sealed trait MyStream[+A]
case object MyEmpty extends MyStream[Nothing]
case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {
  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    MyCons(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = MyEmpty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def headOption: MyOption[A] = this match {
    case MyEmpty => MyNone
    case MyCons(h, t) => MySome(h())
  }
}
