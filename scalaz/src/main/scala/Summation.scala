trait FoldLeft[F[_]] {
  def foldLeft[A, B](xs: F[A], b: B, f: (B, A) => B): B
}

object FoldLeft {
  implicit val FoldLeftList = new FoldLeft[List] {
    def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B): B = xs.foldLeft(b)(f)
  }
}

trait Monoid[A] {
  def mappend(a1: A, a2: A): A
  def mzero: A
}

object Monoid {
  implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
    def mappend(a: Int, b: Int): Int = a + b
    def mzero: Int = 0
  }

  implicit val StringMonoid: Monoid[String] = new Monoid[String] {
    def mappend(a: String, b: String): String = a + b
    def mzero: String = ""
  }
}

trait Identity[A] {
  val value: A

  def |+|(a2: A)(implicit m: Monoid[A]): A = m.mappend(value, a2)
}

trait MA[M[_], A] {
  val value: M[A]

  def summation(implicit m: Monoid[A], fl: FoldLeft[M]): A = fl.foldLeft(value, m.mzero, m.mappend)
}

object Main extends App {
  def sum[M[_], T](xs: M[T])(implicit m: Monoid[T], fl: FoldLeft[M]): T = fl.foldLeft(xs, m.mzero, m.mappend)

  val multMonoid = new Monoid[Int] {
    def mappend(a: Int, b: Int): Int = a * b
    def mzero: Int = 1
  }

  println(sum(List(1, 2, 3, 4)))
  println(sum(List("foo", "bar", "baz")))
  println(sum(List(1, 2, 3, 4))(multMonoid, implicitly[FoldLeft[List]]))

  implicit def toIdent[A](a: A): Identity[A] = new Identity[A] {
    val value = a
  }
  println(3 |+| 4)

  implicit def toMA[M[_], A](ma: M[A]): MA[M, A] = new MA[M, A] {
    val value = ma
  }
  println(List(1, 2, 3, 4, 5).summation)
}
