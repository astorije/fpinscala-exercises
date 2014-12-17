package ch2gettingstarted

import org.scalatest.FunSuite

import GettingStarted._

class GettingStartedSuite extends FunSuite {

  // Exercise 2.1
  test("fib") {
    assert(fib(0) === 0)
    assert(fib(1) === 1)
    assert(fib(2) === 1)
    assert(fib(3) === 2)
    assert(fib(4) === 3)
    assert(fib(5) === 5)
  }

  // Exercise 2.2
  test("isSorted") {
    assert(isSorted(Array(1, 2, 2, 3), (x: Int, y: Int) => x <= y) === true)
    assert(isSorted(Array('a', 'b', 'c'), (x: Char, y: Char) => x <= y) === true)
    assert(isSorted(Array(1, 4, 2, 3), (x: Int, y: Int) => x <= y) === false)
    assert(isSorted(Array('d', 'b', 'c'), (x: Char, y: Char) => x <= y) === false)
  }

  def addTotal(a: Int, b: Int): Int = a + b
  def addPartial(a: Int): Int => Int = (b: Int) => a + b
  def double(a: Int): Int = a * 2

  // Exercise 2.3
  test("curry") {
    assert(curry(addTotal)(1)(2) === 3)
  }

  // Exercise 2.4
  test("uncurry") {
    assert(uncurry(addPartial)(1, 2) === 3)
  }

  // Exercise 2.3 and 2.4
  test("curry and uncurry") {
    assert(curry(uncurry(addPartial))(1)(2) === addPartial(1)(2))
    assert(uncurry(curry(addTotal))(1, 2) === addTotal(1, 2))
  }

  // Exercises 2.5
  test("compose") {
    assert(compose(double, double)(3) === double(double(3)))
  }
}
