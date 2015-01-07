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

  // Exercise 6.6
  // Write the implementation of map2 based on the following signature. This
  // function takes two actions, ra and rb, and a function f for combining their
  // results, and returns a new action that combines them:
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  // Exercise 6.7
  // Hard: If you can combine two RNG transitions, you should be able to combine a whole
  // list of them. Implement sequence for combining a List of transitions into a single
  // transition. Use it to reimplement the ints function you wrote before. For the latter,
  // you can use the standard library function List.fill(n)(x) to make a list with x
  // repeated n times.
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    // rng => fs.foldRight((Nil: List[A], rng))((el, acc) => (el(acc._2)._1 :: acc._1, el(acc._2)._2))
    fs.foldRight(unit(Nil: List[A]))((el, acc) => map2(el, acc)(_ :: _))

  def intsRand(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  // Exercise 6.8
  // Implement flatMap, and then use it to implement nonNegativeLessThan.
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) >= mod) unit(mod)
      else nonNegativeLessThan(n)
    }

  // Exercise 6.9
  // Reimplement map and map2 in terms of flatMap. The fact that this is
  // possible is what we’re referring to when we say that flatMap is more
  // powerful than map and map2.
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    // flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}
