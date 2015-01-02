package ch5laziness

import org.scalatest.FunSuite

import Stream._

class StreamSuite extends FunSuite {

  // Exercise 5.1
  test("toList") {
    assert(Stream(1, 2, 3).toList === List(1, 2, 3))
  }

  // Exercise 5.2
  test("take") {
    assert(Stream(1, 2, 3).take(2).toList === List(1, 2))
  }

  // Exercise 5.2
  test("drop") {
    assert(Stream(1, 2, 3).drop(2).toList === List(3))
  }

  // Exercise 5.3
  test("takeWhile") {
    assert(Stream(1, 2, 3, 4).takeWhile(_ < 3).toList === List(1, 2))
  }
}
