package ch4errorhandling

import org.scalatest.FunSuite

import Either._

class EitherSuite extends FunSuite {

  // Exercise 4.6
  test("map") {
    assert(Right(3).map(_ * 2) === Right(6))
    assert((Left("Error"): Either[String, Int]).map(_ * 2) === Left("Error"))
  }

  // Exercise 4.6
  test("flatMap") {
    assert(Right(42).flatMap(Left(_)) === Left(42))
    assert(Left(42).flatMap(Right(_)) === Left(42))
  }

  // Exercise 4.6
  test("orElse") {
    assert(Right(42).orElse(Right(0)) === Right(42))
    assert(Left("Error").orElse(Right(42)) === Right(42))
  }

  // Exercise 4.6
  test("map2") {
    assert(Right(2).map2(Right(4))(_ * _) === Right(8))
    assert(Right(2).map2(Left("Error"): Either[String, Int])(_ * _) === Left("Error"))
    assert((Left("Error"): Either[String, Int]).map2(Right(4))(_ * _) === Left("Error"))
  }
}
