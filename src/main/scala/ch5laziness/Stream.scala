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

}
