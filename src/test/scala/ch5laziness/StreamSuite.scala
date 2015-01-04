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

  // Exercise 5.8
  test("constant") {
    assert(constant(42).take(3).toList === List(42, 42, 42))
  }

  // Exercise 5.9
  test("from") {
    assert(from(5).take(3).toList === List(5, 6, 7))
  }

  // Exercise 5.10
  test("fibs") {
    assert(fibs.take(7).toList === List(0, 1, 1, 2, 3, 5, 8))
  }

  // Exercise 5.11
  test("unfold") {
    assert(unfold(1)(i => Some(i, i + 1)).take(3).toList === List(1, 2, 3))
  }

  // Exercise 5.12
  test("fibsViaUnfold") {
    assert(fibsViaUnfold.take(7).toList === List(0, 1, 1, 2, 3, 5, 8))
  }

  // Exercise 5.12
  test("fromViaUnfold") {
    assert(fromViaUnfold(5).take(3).toList === List(5, 6, 7))
  }

  // Exercise 5.12
  test("constantViaUnfold") {
    assert(constantViaUnfold(42).take(3).toList === List(42, 42, 42))
  }

  // Exercise 5.12
  test("onesViaUnfold") {
    assert(onesViaUnfold.take(3).toList === List(1, 1, 1))
  }

  // Exercise 5.13
  test("mapViaUnfold") {
    assert(Stream(1, 2, 3).mapViaUnfold(_ * 2).toList === List(2, 4, 6))
    assert(empty[Int].mapViaUnfold(_ * 2).toList === Nil)
  }

  // Exercise 5.13
  test("takeViaUnfold") {
    assert(Stream(1, 2, 3).takeViaUnfold(2).toList === List(1, 2))
    assert(Stream(1, 2, 3).takeViaUnfold(5).toList === List(1, 2, 3))
    assert(Stream(1, 2, 3).takeViaUnfold(0).toList === Nil)
  }

  // Exercise 5.13
  test("takeWhileViaUnfold") {
    assert(Stream(1, 2, 3, 4).takeWhileViaUnfold(_ < 3).toList === List(1, 2))
  }

  // Exercise 5.13
  test("zipWithViaUnfold") {
    assert(Stream(1, 2).zipWithViaUnfold(Stream(3, 4))(_ + _).toList === List(4, 6))
    assert(Stream(1, 2).zipWithViaUnfold(empty)(_ + _).toList === Nil)
    assert(empty[Int].zipWithViaUnfold(Stream(3, 4))(_ + _).toList === Nil)
  }

  // Exercise 5.13
  test("zipAllViaUnfold") {
    assert(Stream(1, 2).zipAllViaUnfold(Stream(4)).toList === List((Some(1), Some(4)), (Some(2), None)))
    assert(Stream(1, 2).zipAllViaUnfold(empty).toList === List((Some(1), None), (Some(2), None)))
    assert(empty.zipAllViaUnfold(empty).toList === Nil)
  }

  // Exercise 5.14
  test("startsWith") {
    assert(Stream(1,2,3).startsWith(Stream(1,2)) === true)
    assert(Stream(1,2,3).startsWith(Stream(42)) === false)
    assert(empty.startsWith(Stream(42)) === false)
    assert(Stream(1,2,3).startsWith(empty) === true)
    assert(empty.startsWith(empty) === true)
  }

  // Exercise 5.15
  test("tails") {
    assert(Stream(1,2,3).tails.map(_.toList).toList === List(List(1,2,3), List(2,3), List(3), List()))
    assert(empty[Any].tails.map(_.toList).toList === List(Nil))
  }

  // Exercise 5.16
  test("scanRight") {
    assert(Stream(1,2,3).scanRight(0)(_ + _).toList === List(6,5,3,0))
  }
}
