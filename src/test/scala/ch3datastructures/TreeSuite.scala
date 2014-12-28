package ch3datastructures

import org.scalatest.FunSuite

import Tree._

class TreeSuite extends FunSuite {

  // Exercise 3.25
  test("size") {
    assert(size(Leaf('a')) === 1)
    assert(size(Branch(Leaf(2), Branch(Branch(Leaf(5), Leaf(6)), Leaf(7)))) === 7)
  }

  // Exercise 3.26
  test("maximum") {
    assert(maximum(Leaf(5)) === 5)
    assert(maximum(Branch(Leaf(4), Branch(Leaf(12), Leaf(7)))) === 12)
  }

  // Exercise 3.27
  test("depth") {
    assert(depth(Branch(Leaf(4), Branch(Leaf(12), Leaf(7)))) === 2)
  }

  // Exercise 3.28
  test("map") {
    assert(
      map(Branch(Leaf(4), Branch(Leaf(12), Leaf(7))))(_ + 2) ===
      Branch(Leaf(6), Branch(Leaf(14), Leaf(9))))
  }
}
