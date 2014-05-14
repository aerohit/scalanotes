package fppinscala.chap3

sealed trait MyList[+A]
case object MyNil extends MyList[Nothing]
case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def sum(list: MyList[Int]): Int = list match {
    case MyNil => 0
    case MyCons(head, tail) => head + sum(tail)
  }

  def sumFR(list: MyList[Int]): Int = foldRightDefault(list, 0)(_ + _)

  def sumFL(list: MyList[Int]): Int = foldLeftDefault(list, 0)(_ + _)

  def product(list: MyList[Int]): Int = list match {
    case MyNil => 1
    case MyCons(0, _) => 0
    case MyCons(head, tail) => head * product(tail)
  }

  def productFR(list: MyList[Int]): Int = foldRightDefault(list, 1)(_ * _)

  def productFL(list: MyList[Int]): Int = foldLeftDefault(list, 1)(_ * _)

  // Ex 2
  def tail[A](list: MyList[A]): MyList[A] = list match {
    case MyNil => throw new RuntimeException
    case MyCons(_, t) => t
  }

  // Ex 3
  def setHead[A](list: MyList[A], a: A): MyList[A] = list match {
    case MyNil => throw new RuntimeException
    case MyCons(_, t) => MyCons(a, t)
  }

  // Ex 4
  def drop[A](list: MyList[A], n: Int): MyList[A] =
    if (n <= 0) list
    else list match {
      case MyNil => throw new RuntimeException
      case MyCons(_, t) => drop(t, n - 1)
    }

  // Ex 5
  def dropWhile[A](list: MyList[A], p: A => Boolean): MyList[A] = list match {
    case MyNil => MyNil
    case l @ MyCons(h, t) => if (p(h)) dropWhile(t, p) else l
  }

  def dropWhileCurried[A](list: MyList[A])(p: A => Boolean): MyList[A] = list match {
    case MyNil => MyNil
    case l @ MyCons(h, t) => if (p(h)) dropWhile(t, p) else l
  }

  def append[A](list1: MyList[A], list2: MyList[A]): MyList[A] = list1 match {
    case MyNil => list2
    case MyCons(h, t) => MyCons(h, append(t, list2))
  }

  def appendFL[A](list1: MyList[A], list2: MyList[A]): MyList[A] =
    ???

  def appendFR[A](list1: MyList[A], list2: MyList[A]): MyList[A] =
    foldRightDefault(list1, list2)((a, l) => MyCons(a, l))

  // Ex 6
  def init[A](list: MyList[A]): MyList[A] = list match {
    case MyNil => throw new RuntimeException
    case MyCons(h, MyNil) => MyNil
    case MyCons(h, t) => MyCons(h, init(t))
  }

  def foldRightDefault[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    foldRightNTR(as, z)(f)

  @annotation.tailrec
  def foldRightTR[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
    case MyNil => z
    case MyCons(h, t) => foldRightTR(t, f(h, z))(f)
  }

  def foldRightNTR[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
    case MyNil => z
    case MyCons(h, t) => f(h, foldRightNTR(t, z)(f))
  }

  def foldRightUsingFL[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    foldLeftDefault(as, z)((b, a) => f(a, b))

  def lengthFR[A](list: MyList[A]): Int = foldRightDefault(list, 0)((_, b) => b + 1)

  def lengthFL[A](list: MyList[A]): Int = foldLeftDefault(list, 0)((b, _) => b + 1)

  def foldLeftDefault[A, B](as: MyList[A], z: B)(f: (B, A) => B): B =
    foldLeftTR(as, z)(f)

  @annotation.tailrec
  def foldLeftTR[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = as match {
    case MyNil => z
    case MyCons(h, t) => foldLeftTR(t, f(z, h))(f)
  }

  def foldLeftNTR[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = as match {
    case MyNil => z
    case MyCons(h, t) => f(foldLeftNTR(t, z)(f), h)
  }

  def foldLeftUsingFR[A, B](as: MyList[A], z: B)(f: (B, A) => B): B =
    foldRightDefault(as, z)((a,b) => f(b, a))

  def reverseFL[A](list: MyList[A]) = foldLeftDefault(list, MyList[A]())((l, a) => append(MyList(a), l))

  def reverseFR[A](list: MyList[A]) = foldRightDefault(list, MyList[A]())((a, l) => append(l, MyList(a)))

  def concatenate[A](lists: MyList[MyList[A]]): MyList[A] =
    foldRightDefault(lists, MyList[A]())((l, ls) => append(l, ls))

  def mapS[A, B](list: MyList[A])(f: A => B): MyList[B] = list match {
    case MyNil => MyNil
    case MyCons(h, t) => MyCons(f(h), mapS(t)(f))
  }

  def filter[A](list: MyList[A])(p: A => Boolean): MyList[A] = list match {
    case MyNil => MyNil
    case MyCons(h, t) => if (p(h)) MyCons(h, filter(t)(p)) else filter(t)(p)
  }

  def flatMap[A, B](list: MyList[A])(f: A => MyList[B]): MyList[B] = list match {
    case MyNil => MyNil
    case MyCons(h, t) => append(f(h), flatMap(t)(f))
  }

  def filterUsingFM[A](list: MyList[A])(p: A => Boolean): MyList[A] =
    flatMap(list)(a => if(p(a)) MyList(a) else MyNil)

  def sumCorrespondingElements(list1: MyList[Int], list2: MyList[Int]): MyList[Int] = (list1, list2) match {
    case (_, MyNil) => MyNil
    case (MyNil, _) => MyNil
    case (MyCons(h1, t1), MyCons(h2, t2)) => MyCons(h1 + h2, sumCorrespondingElements(t1, t2))
  }

  def combine[A, B, C](l1: MyList[A], l2: MyList[B])(f: (A, B) => C): MyList[C] = (l1, l2) match {
    case (_, MyNil) => MyNil
    case (MyNil, _) => MyNil
    case (MyCons(h1, t1), MyCons(h2, t2)) => MyCons(f(h1, h2), combine(t1, t2)(f))
  }

  def take[A](l: MyList[A])(n: Int): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(h, t) =>
      if (n <= 0)
        MyNil
      else
        MyCons(h, take(t)(n-1))
  }

  def takeWhile[A](l: MyList[A])(p: A => Boolean): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(h, t) =>
      if(p(h))
        MyCons(h, takeWhile(t)(p))
      else
        MyNil
  }

  def myForAll[A](l: MyList[A])(p: A => Boolean): Boolean = l match {
    case MyNil => true
    case MyCons(h, t) =>
      if(p(h))
        myForAll(t)(p)
      else false
  }

  def exists[A](l: MyList[A])(p: A => Boolean): Boolean = l match {
    case MyNil => false
    case MyCons(h, t) =>
      if(p(h))
        true
      else
        exists(t)(p)
  }

  // is it possible to make it tail recursive?
  def scanLeft[A, B](l: MyList[A], z: B)(f: (A, B) => B): MyList[B] = l match {
    case MyNil => MyList(z)
    case MyCons(h, t) => MyCons(z, scanLeft(t, f(h, z))(f))
  }

  // is it possible to make it tail recursive?
  def scanRight[A, B](l: MyList[A], z: B)(f: (B, A) => B): MyList[B] = l match {
    case MyNil => MyList(z)
    case MyCons(h, t) => MyCons(f(z, h), scanRight(t, z)(f))
  }

  // Ex 24
  def hasSubsequence[A](list: MyList[A], sublist: MyList[A]): Boolean = {
    def checkSubsequence(l: MyList[A]): Boolean = (l, sublist) match {
      case (MyNil, MyNil) => true
      case (MyNil, _) => false
      case (_, MyNil) => true
      case (MyCons(lh, lt), MyCons(sh, st)) =>
        if (lh == sh)
          hasSubsequence(lt, st)
        else false
    }
    false
  }

  def apply[A](elements: A*): MyList[A] =
    if (elements.isEmpty) MyNil
    else MyCons(elements.head, apply(elements.tail: _*))
}
