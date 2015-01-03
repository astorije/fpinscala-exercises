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

  // Exercise 5.4
  test("forAll") {
    assert(Stream(1, 2, 3).forAll(_ > 0) === true)
    assert(Stream(1, 2, -3).forAll(_ > 0) === false)
  }

  // Exercise 5.5
  test("takeWhileViaFoldRight") {
    assert(Stream(1, 2, 3, 4).takeWhileViaFoldRight(_ < 3).toList === List(1, 2))
  }

  // Exercise 5.6
  test("headOptionViaFoldRight") {
    assert(Stream(1, 2, 3).headOptionViaFoldRight === Some(1))
    assert(empty.headOptionViaFoldRight === None)
  }

  // Exercise 5.7
  test("map") {
    assert(Stream(1, 2, 3).map(_ * 2).toList === List(2, 4, 6))
    assert(empty[Int].map(_ * 2).toList === Nil)
  }

  // Exercise 5.7
  test("filter") {
    assert(Stream(1, 2, 3).filter(_ % 2 == 0).toList === List(2))
    assert(empty[Int].filter(_ > 0).toList === Nil)
  }

  // Exercise 5.7
  test("append") {
    assert(Stream(1, 2).append(Stream(3, 4)).toList === List(1, 2, 3, 4))
    assert(empty.append(Stream(0)).toList === List(0))
    assert(Stream(1, 2).append(empty).toList === List(1, 2))
  }

  // Exercise 5.7
  test("flatMap") {
    assert(Stream(1, 2, 3).flatMap(Stream(0, _)).toList === List(0, 1, 0, 2, 0, 3))
  }
}
