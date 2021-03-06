package fppinscala.chap5

import fppinscala.chap4.{MyNone, MyOption, MySome}

sealed trait MyStream[+A] {
  def headOption: MyOption[A] = this match {
    case MyEmpty => MyNone
    case MyCons(h, t) => MySome(h())
  }

  // this would stack overflow
  def toListRecursive: List[A] = this match {
    case MyEmpty => Nil
    case MyCons(h, t) => h() :: t().toListRecursive
  }

  // this is expensive
  def toListTailRecursive: List[A] = {
    @annotation.tailrec
    def go(s: MyStream[A], acc: List[A]): List[A] = s match {
      case MyEmpty => acc
      case MyCons(h, t) => go(t(), h() :: acc)
    }
    go(this, Nil).reverse
  }

  // this is fast
  def toListTailRecursiveFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: MyStream[A]): List[A] = s match {
      case MyEmpty => buf.toList
      case MyCons(h, t) =>
        buf += h()
        go(t())
    }
    go(this)
  }

  def toList: List[A] = toListTailRecursiveFast

  def take(n: Int): MyStream[A] = {
    if (n > 0) this match {
      case MyCons(h, t) => MyStream.cons(h(), t().take(n - 1))
      case MyEmpty => MyEmpty
    } else {
      MyEmpty
    }
  }

  def takeWhile(p: A => Boolean): MyStream[A] = this match {
    case MyCons(h, t) if p(h()) => MyStream.cons(h(), t() takeWhile p)
    case _ => MyEmpty
  }
}

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
}
