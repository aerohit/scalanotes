package fppinscala.chap3

sealed trait MyTree[+A]

case class MyLeaf[A](value: A) extends MyTree[A]

case class MyBranch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {
  def treeSize[A](tree: MyTree[A]): Int = tree match {
    case MyLeaf(_) => 1
    case MyBranch(l, r) => 1 + treeSize(l) + treeSize(r)
  }

  def maximum[A](tree: MyTree[A])(implicit comp: (A, A) => A): A = tree match {
    case MyLeaf(v) => v
    case MyBranch(l, r) => comp(maximum(l), maximum(r))
  }

  def depth[A](tree: MyTree[A]): Int = tree match {
    case MyLeaf(v) => 0
    case MyBranch(l, r) => 1 + (depth(l) max depth(r))
  }

  def treeMap[A, B](tree: MyTree[A])(f: A => B): MyTree[B] = tree match {
    case MyLeaf(v) => MyLeaf(f(v))
    case MyBranch(l, r) => MyBranch(treeMap(l)(f), treeMap(r)(f))
  }
}
