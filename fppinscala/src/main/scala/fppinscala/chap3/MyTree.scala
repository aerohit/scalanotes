package fppinscala.chap3

sealed trait MyTree[+A]

case class MyLeaf[A](value: A) extends MyTree[A]

case class MyBranch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {
  def treeSize[A](tree: MyTree[A]): Int = tree match {
    case MyLeaf(_) => 1
    case MyBranch(l, r) => 1 + treeSize(l) + treeSize(r)
  }

  def treeSizeF[A](tree: MyTree[A]): Int =
    fold(tree, (_: A) => 1)((l, r) => 1 + treeSizeF(l) + treeSizeF(r))

  def maximum[A](tree: MyTree[A])(implicit comp: (A, A) => A): A = tree match {
    case MyLeaf(v) => v
    case MyBranch(l, r) => comp(maximum(l), maximum(r))
  }

  def maximumF[A](tree: MyTree[A])(implicit comp: (A, A) => A): A =
    fold(tree, (v: A) => v)((l, r) => comp(maximumF(l), maximumF(r)))

  def depth[A](tree: MyTree[A]): Int = tree match {
    case MyLeaf(v) => 0
    case MyBranch(l, r) => 1 + (depth(l) max depth(r))
  }

  def depthF[A](tree: MyTree[A]): Int =
    fold(tree, (_: A) => 0)((l, r) => 1 + (depthF(l) max depthF(r)))

  def treeMap[A, B](tree: MyTree[A])(f: A => B): MyTree[B] = tree match {
    case MyLeaf(v) => MyLeaf(f(v))
    case MyBranch(l, r) => MyBranch(treeMap(l)(f), treeMap(r)(f))
  }

  def treeMapF[A, B](tree: MyTree[A])(f: A => B): MyTree[B] =
    fold[A, MyTree[B]](tree, (v: A) => MyLeaf(f(v)))((l, r) => MyBranch(treeMapF(l)(f), treeMapF(r)(f)))

  def fold[A, B](tree: MyTree[A], z: A => B)(f: (MyTree[A], MyTree[A]) => B): B = tree match {
    case MyLeaf(v) => z(v)
    case MyBranch(l, r) => f(l, r)
  }
}
