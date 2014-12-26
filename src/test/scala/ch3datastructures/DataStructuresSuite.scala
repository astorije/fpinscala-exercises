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
}
