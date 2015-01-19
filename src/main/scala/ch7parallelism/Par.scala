package ch7parallelism

import java.util.concurrent._

object Par {
  // Exercise 7.2
  // Before continuing, try to come up with representations for Par that make it
  // possible to implement the functions of our API.
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = ???

  def fork[A](a: => Par[A]): Par[A] = ???

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // Exercise 7.1
  // Par.map2 is a new higher-order function for combining the result of two
  // parallel computations. What is its signature? Give the most general
  // signature possible (don’t assume it works only for Int).
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???
}
