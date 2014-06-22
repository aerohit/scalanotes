package fppinscala.chap5

import org.specs2.mutable.Specification

import MyLazy._

class MyLazyTest extends Specification {
  "if2 should return the first argument if true" in {
    if2(true, 2, 2/0) mustEqual 2
  }
  "if2 should return the second argument if false" in {
    if2(false, 2/0, 2) mustEqual 2
  }
}

