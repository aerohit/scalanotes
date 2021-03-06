package fppinscala.chap4

sealed trait MyEither[+E, +A] {
  def mapR[B](f: A => B): MyEither[E, B] = this match {
    case MyLeft(l) => MyLeft(l)
    case MyRight(r) => MyRight(f(r))
  }

  def flatMapR[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(l) => MyLeft(l)
    case MyRight(r) => f(r)
  }

  def orElseR[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(_) => b
    case MyRight(r) => MyRight(r)
  }

  def map2R[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = {
    this flatMapR (aa => b mapR (bb => f(aa, bb)))
  }
}
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

  def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = as match {
    case Nil => MyRight(Nil)
    case x :: xs => (f(x) map2R traverse(xs)(f))(_ :: _)
  }

  def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
    traverse(es)(x => x)

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): MyEither[String, Name] =
    if (name == null || name == "") MyLeft("name is empty")
    else MyRight(new Name(name))

  def mkAge(age: Int): MyEither[String, Age] =
    if (age < 0) MyLeft("Age is out of range")
    else MyRight(new Age(age))

  def mkPerson(name: String, age: Int): MyEither[String, Person] =
    mkName(name).map2R(mkAge(age))(Person(_, _))
}
