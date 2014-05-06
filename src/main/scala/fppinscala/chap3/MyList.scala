package fppinscala.chap3

sealed trait MyList[+A]
case object MyNil extends MyList[Nothing]
case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def sum(list: MyList[Int]): Int = list match {
    case MyNil => 0
    case MyCons(head, tail) => head + sum(tail)
  }

  def product(list: MyList[Int]): Int = list match {
    case MyNil => 1
    case MyCons(0, _) => 0
    case MyCons(head, tail) => head * product(tail)
  }
  
  def tail[A](list: MyList[A]): MyList[A] = list match {
    case MyNil => throw new RuntimeException
    case MyCons(_, t) => t
  }

  def setHead[A](list: MyList[A], a: A): MyList[A] = list match {
    case MyNil => throw new RuntimeException
    case MyCons(_, t) => MyCons(a, t)
  }

  def apply[A](elements: A*): MyList[A] =
    if (elements.isEmpty) MyNil
    else MyCons(elements.head, apply(elements.tail: _*))
}
