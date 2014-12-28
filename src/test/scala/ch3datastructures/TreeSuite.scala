package ch3datastructures

import org.scalatest.FunSuite

import Tree._

class TreeSuite extends FunSuite {

  test("size") {
    assert(size(Leaf('a')) === 1)
    assert(size(Branch(Leaf(2), Branch(Branch(Leaf(5), Leaf(6)), Leaf(7)))) === 7)
  }

  test("maximum") {
    assert(maximum(Leaf(5)) === 5)
    assert(maximum(Branch(Leaf(4), Branch(Leaf(12), Leaf(7)))) === 12)
  }

  test("depth") {
    assert(depth(Branch(Leaf(4), Branch(Leaf(12), Leaf(7)))) === 3)
  }

  // Exercise 3.28
  test("map") {
    assert(
      map(Branch(Leaf(4), Branch(Leaf(12), Leaf(7))))(_ + 2) ===
      Branch(Leaf(6), Branch(Leaf(14), Leaf(9))))
  }
}
