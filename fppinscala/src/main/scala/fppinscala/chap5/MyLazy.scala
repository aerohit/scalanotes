package fppinscala.chap5

object MyLazy {
  def if2[A](condition: Boolean, onTrue: => A, onFalse: => A): A =
    if (condition) onTrue else onFalse
}
