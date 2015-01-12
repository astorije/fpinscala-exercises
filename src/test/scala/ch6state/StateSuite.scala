package ch6state

import org.scalatest.FunSuite

import RNG._
import CandyDispenser._

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

  // Exercise 6.7
  test("sequence") {
    assert(sequence(List(unit(1), unit(2), unit(3)))(RNG.Simple(0L))._1 === List(1, 2, 3))
  }

  // Exercise 6.7
  test("intsRand") {
    assert(intsRand(3)(RNG.Simple(42L)) === ints(3)(RNG.Simple(42L)))
  }

  // Exercise 6.8
  test("flatMap") {
    assert(flatMap(unit("foo"))(a => unit(a + "bar"))(RNG.Simple(0L))._1 === "foobar")
  }

  // Exercise 6.8
  test("nonNegativeLessThan") {
    assert(nonNegativeLessThan(2)(RNG.Simple(42L))._1 === 1)
  }

  // Exercise 6.9
  test("map2ViaFlatMap") {
    assert(map2ViaFlatMap(unit(1), unit(2))(_ + _)(RNG.Simple(0L))._1 === 3)
    assert(map2ViaFlatMap(nonNegativeInt, double)((_, _))(RNG.Simple(42L)) === intDouble(RNG.Simple(42L)))
  }

  // Exercise 6.9
  test("mapViaFlatMap") {
    assert(mapViaFlatMap(unit(1))(_ * 2)(RNG.Simple(0L)) === map(unit(1))(_ * 2)(RNG.Simple(0L)))
  }

  // Exercise 6.10
  test("State.unit") {
    assert(State.unit(1).run(42) === (1, 42))
  }

  // Exercise 6.10
  test("State.sequence") {
    assert(State.sequence[Int, Int](List(State.unit(1), State.unit(2))).run(42) === (List(1, 2), 42))
  }

  // Exercise 6.10
  test("State.map") {
    assert(State[Int, Int](s => (2, s + 1)).map(_ * 3).run(0) === (6, 1))
  }

  // Exercise 6.10
  test("State.map2") {
    var s = State[Int, Int](s => (2, s + 1))
    assert(s.map2(s)(_ * _).run(0) === (4, 2))
  }

  // Exercise 6.10
  test("State.flatMap") {
    assert(State[Int, Int](s => (5, s + 1)).flatMap(a => State.unit(a)).run(0) === (5, 1))
  }

  // Exercise 6.11
  test("simulateMachine: respect rules (machine state)") {
    val lockedMachine = Machine(true, 10, 20)
    val unlockedMachine = Machine(false, 10, 20)
    val emptyLockedMachine = Machine(true, 0, 20)
    val emptyUnlockedMachine = Machine(false, 0, 20)

    // Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
    assert(simulateMachine(List(Coin)).run(lockedMachine)._2.locked === false)

    // Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
    assert(simulateMachine(List(Turn)).run(unlockedMachine)._2.locked === true)
    assert(simulateMachine(List(Turn)).run(unlockedMachine)._2.candies === 9)

    // Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
    assert(simulateMachine(List(Turn)).run(lockedMachine)._2 === lockedMachine)
    assert(simulateMachine(List(Coin)).run(unlockedMachine)._2 === unlockedMachine)

    // A machine that’s out of candy ignores all inputs.
    assert(simulateMachine(List(Turn)).run(emptyLockedMachine)._2 === emptyLockedMachine)
    assert(simulateMachine(List(Turn)).run(emptyUnlockedMachine)._2 === emptyUnlockedMachine)
    assert(simulateMachine(List(Coin)).run(emptyLockedMachine)._2 === emptyLockedMachine)
    assert(simulateMachine(List(Coin)).run(emptyUnlockedMachine)._2 === emptyUnlockedMachine)
  }

  // Exercise 6.11
  test("simulateMachine: return value") {
    assert(simulateMachine(List(Coin)).run(Machine(true, 5, 10))._1 === (11, 5))
    assert(simulateMachine(List(Turn)).run(Machine(true, 5, 10))._1 === (10, 5))
    assert(simulateMachine(List(Coin, Turn)).run(Machine(true, 5, 10))._1 === (11, 4))
    assert(simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 5, 10))._1 === (14, 1))
  }
}
