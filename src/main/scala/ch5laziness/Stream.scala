package ch5laziness

import Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // Exercise 5.1
  // Write a function to convert a Stream to a List, which will force its
  // evaluation and let you look at it in the REPL. You can convert to the
  // regular List type in the standard library. You can place this and other
  // functions that operate on a Stream inside the Stream trait.
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  // Exercise 5.2
  // Write the function take(n) for returning the first n elements of a Stream,
  // and drop(n) for skipping the first n elements of a Stream.
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // Exercise 5.3
  // Write the function takeWhile for returning all starting elements of a
  // Stream that match the given predicate.
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  // Exercise 5.4
  // Implement forAll, which checks that all elements in the Stream match a given
  // predicate. Your implementation should terminate the traversal as soon as it
  // encounters a nonmatching value.
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, r) => p(h) && r)
    // this match {
    //   case Cons(h, _) if !p(h()) => false
    //   case Cons(_, t) => t() forAll p
    //   case _ => true
    // }

  // Exercise 5.5
  // Use foldRight to implement takeWhile.
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((h, t) => if (p(h)) cons(h, t) else empty)

  // Exercise 5.6
  // Hard: Implement headOption using foldRight.
  def headOptionViaFoldRight: Option[A] =
    foldRight[Option[A]](None)((h, _) => Some(h))

  // Exercise 5.7
  // Implement map, filter, append, and flatMap using foldRight. The append method
  // should be non-strict in its argument.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h, t)
      else t
    )

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // Exercise 5.8
  // Generalize ones slightly to the function constant, which returns an infinite
  // Stream of a given value.
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // Exercise 5.9
  // Write a function that generates an infinite stream of integers, starting
  // from n, then n + 1, n + 2, and so on.
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // Exercise 5.10
  // Write a function fibs that generates the infinite stream of Fibonacci numbers:
  // 0, 1, 1, 2, 3, 5, 8, and so on.
  def fibs: Stream[Int] = {
    def loop(previous: Int, current: Int): Stream[Int] =
      cons(previous, loop(current, previous + current))
    loop(0, 1)
  }

  // Exercise 5.11
  // Write a more general stream-building function called unfold. It takes an
  // initial state, and a function for producing both the next state and the
  // next value in the generated stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }
}
