package ch6state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Exercise 6.1
  // Write a function that uses RNG.nextInt to generate a random integer between
  // 0 and Int.maxValue (inclusive). Make sure to handle the corner case when
  // nextInt returns Int.MinValue, which doesn’t have a non-negative counterpart.
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (i, r) if i < 0 => (-i-1, r)
    case t => t
  }

  // Exercise 6.2
  // Write a function to generate a Double between 0 and 1, not including 1.
  // Note: You can use Int.MaxValue to obtain the maximum positive integer value,
  // and you can use x.toDouble to convert an x: Int to a Double.
  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (i, r) => ((i / (Int.MaxValue.toDouble + 1)), r)
  }

  // Exercise 6.3
  // Write functions to generate an (Int, Double) pair, a (Double, Int) pair,
  // and a (Double, Double, Double) 3-tuple. You should be able to reuse the
  // functions you’ve already written.
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // Exercise 6.4
  // Write a function to generate a list of random integers.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      (Nil, rng)
    else {
      val (i, r1) = rng.nextInt
      val (l, r2) = ints(count - 1)(r1)
      (i :: l, r2)
    }
  }

  // Exercise 6.5
  // Use map to reimplement double in a more elegant way. See exercise 6.2.
  def doubleRand: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
}
