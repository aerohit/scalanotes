package fppinscala.chap4

import fppinscala.chap3._

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(v) => MySome(f(v))
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(v) => f(v)
  }

  def flatMapUsingMap[B](f: A => MyOption[B]): MyOption[B] =
    map(f) getOrElse MyNone

  def getOrElse[B >: A](default: => B): B = this match {
    case MyNone => default
    case MySome(v) => v
  }

  def orElse[B >: A](default: => MyOption[B]): MyOption[B] = this match {
    case MyNone => default
    case x => x
  }

  def orElseUningMap[B >: A](default: => MyOption[B]): MyOption[B] =
    map (MySome(_)) getOrElse default

  def filter(p: A => Boolean): MyOption[A] = this match {
    case MyNone => MyNone
    case x@MySome(v) =>
      if (p(v)) x
      else MyNone
  }

  def filterUsingFlatMap(p: A => Boolean): MyOption[A] =
    flatMap (a => if (p(a)) this else MyNone)
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

  def lift[A, B](f: A => B): MyOption[A] => MyOption[B] = _ map f

  def myTry[A](a: => A): MyOption[A] =
    try MySome(a)
    catch { case e: Exception => MyNone}

  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = (a, b) match {
    case (MySome(x), MySome(y)) => MySome(f(x, y))
    case _ => MyNone
  }

  def map2UsingFlatMap[A, B, C](aOpt: MyOption[A], bOpt: MyOption[B])(f: (A, B) => C): MyOption[C] =
    aOpt flatMap (a => bOpt map (b => f(a, b)))

  def sequence[A](list: MyList[MyOption[A]]): MyOption[MyList[A]] =
    traverse(list)(x => x)

  def traverse[A, B](list: MyList[A])(f: A => MyOption[B]): MyOption[MyList[B]] = list match {
    case MyNil => MySome(MyList())
    case MyCons(h, t) => f(h) flatMap (hh => traverse(t)(f) map (tt => MyCons(hh, tt)))
  }

  def traverseUsingMap2[A, B](list: MyList[A])(f: A => MyOption[B]): MyOption[MyList[B]] = list match {
    case MyNil => MySome(MyList())
    case MyCons(h, t) => map2(f(h), traverseUsingMap2(t)(f))(MyCons(_, _))
  }

  def insuranceRateQuote(age: Int, speedingTickets: Int): Double = age * speedingTickets / 100.0

  def parseInsuranceRateQuote(age: String, speedingTickets: String): MyOption[Double] = {
    val ageOpt = myTry(age.toInt)
    val ticketsOpt = myTry(speedingTickets.toInt)
    map2(ageOpt, ticketsOpt)(insuranceRateQuote)
  }

  def parseInts(list: MyList[String]): MyOption[MyList[Int]] =
    traverse(list)(x => myTry(x.toInt))
}
