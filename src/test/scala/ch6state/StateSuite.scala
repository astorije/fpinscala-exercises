package ch6state

import org.scalatest.FunSuite

import RNG._

class StateSuite extends FunSuite {
  // Exercise 6.1
  test("nonNegativeInt") {
    assert(nonNegativeInt(RNG.Simple(42L)) === (16159453, Simple(1059025964525L)))
  }

  // Exercise 6.2
  test("double") {
    assert(double(RNG.Simple(42L))._1 === 0.007524831686168909)
  }

  // Exercise 6.3
  test("intDouble") {
    assert(intDouble(RNG.Simple(42L))._1 === (16159453, 0.5967354848980904))
  }

  // Exercise 6.3
  test("doubleInt") {
    assert(doubleInt(RNG.Simple(42L))._1 === (0.5967354848980904, 16159453))
  }

  // Exercise 6.3
  test("double3") {
    assert(double3(RNG.Simple(42L))._1 === (0.007524831686168909, 0.5967354848980904, 0.15846728393808007))
  }

  // Exercise 6.4
  test("ints") {
    assert(ints(3)(RNG.Simple(42L))._1 === List(16159453, -1281479697, -340305902))
  }

  // Exercise 6.5
  test("doubleRand") {
    assert(doubleRand(RNG.Simple(42L)) === doubleRand(RNG.Simple(42L)))
  }

  // Exercise 6.6
  test("map2") {
    assert(map2(unit(1), unit(2))(_ + _)(RNG.Simple(0L))._1 === 3)
    assert(map2(nonNegativeInt, double)((_, _))(RNG.Simple(42L)) === intDouble(RNG.Simple(42L)))
  }
}
