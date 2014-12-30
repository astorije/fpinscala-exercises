package ch4errorhandling

import org.scalatest.FunSuite

import Option._

class OptionSuite extends FunSuite {

  // Exercise 4.1
  test("map") {
    assert(Some(3).map(_ * 2) === Some(6))
    assert((None: Option[Int]).map(_ * 2) === None)
  }

  // Exercise 4.1
  test("getOrElse") {
    assert(Some(42).getOrElse(13) === 42)
    assert(None.getOrElse(13) === 13)
  }

  // Exercise 4.1
  test("flatMap") {
    assert(Some(42).flatMap(Some(_)) === Some(42))
    assert(None.flatMap(Some(_)) === None)
  }

  // Exercise 4.1
  test("orElse") {
    assert(Some(42).orElse(Some(13)) === Some(42))
    assert(None.orElse(Some(13)) === Some(13))
  }

  // Exercise 4.1
  test("filter") {
    assert(Some(42).filter(_ > 0) === Some(42))
    assert(Some(-42).filter(_ > 0) === None)
    assert((None: Option[Int]).filter(_ > 0) === None)
  }

  // Exercise 4.2
  test("variance") {
    val s = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
    assert(mean(s) === Some(3.0))
    assert(variance(s) === Some(2.0))
    assert(variance(Seq()) === None)
  }

  // Exercise 4.3
  test("map2") {
    assert(map2(Some(3), Some(5))(_ + _) === Some(8))
    assert(map2(Some(42), None)(_ + _) === None)
  }
}
