package ch5laziness

import org.scalatest.FunSuite

import Stream._

class StreamSuite extends FunSuite {

  // Exercise 5.1
  test("toList") {
    assert(Stream(1, 2, 3).toList === List(1, 2, 3))
  }
}
