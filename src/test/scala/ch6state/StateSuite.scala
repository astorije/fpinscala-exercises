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
}
