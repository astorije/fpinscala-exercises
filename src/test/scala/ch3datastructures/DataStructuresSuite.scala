package ch3datastructures

import org.scalatest.FunSuite

import List._

class DataStructureSuite extends FunSuite {

  // Exercise 3.1
  test("val x") {
    assert(x === 1 + 2)
  }

  // Exercise 3.2
  test("tail") {
    assert(tail(List(1, 2, 3)) === List(2, 3))
    assert(tail(Nil) === Nil)
  }

  // Exercise 3.3
  test("setHead") {
    assert(setHead(0, List(1, 2, 3)) === List(0, 2, 3))
  }

  // Exercise 3.4
  test("drop") {
    assert(drop(List(1, 2, 3, 4), 2) === List(3, 4))
  }

  // Exercise 3.5
  test("dropWhile") {
    assert(dropWhile(List(-2, -1, 0, 1, 2), ((x: Int) => x < 0)) === List(0, 1, 2))
  }

  // Exercise 3.6
  test("init") {
    assert(init(List(1, 2, 3, 4)) === List(1, 2, 3))
  }

  // Exercise 3.9
  test("length") {
    assert(length(List(1, 2, 3)) === 3)
    assert(length(Nil) === 0)
  }

  test("foldLeft") {
    assert(foldLeft(List(1, 2, 3), 0)(_ + _) === 6)
  }

  test("sumLeft") {
    assert(sumLeft(List(12, 13, 14)) === 39)
  }

  test("productLeft") {
    assert(productLeft(List(2, 4, 6)) === 48)
    assert(productLeft(List(2, 4, 0)) === 0)
  }

  test("lengthLeft") {
    assert(lengthLeft(List(1, 2, 3)) === 3)
    assert(lengthLeft(Nil) === 0)
  }

  test("reverse") {
    assert(reverse(List(1, 2, 3)) === List(3, 2, 1))
  }

  test("foldLeftViaFoldRight") {
    assert(foldLeftViaFoldRight(List(1, 2, 3), 0)(_ + _) === 6)
  }

  test("foldRightViaFoldLeft") {
    assert(foldRightViaFoldLeft(List(1, 2, 3), 0)(_ + _) === 6)
  }

  test("appendFold") {
    assert(appendFold(List(1, 2, 3), List(4, 5)) === List(1, 2, 3, 4, 5))
  }

  test("concat") {
    assert(concat(List(List(1), List(2, 3, 4), Nil)) === List(1, 2, 3, 4))
  }
}
