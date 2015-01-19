package ch7parallelism

import java.util.concurrent._

object Par {
  type Par[A] = Any

  def unit[A](a: A): Par[A] = ???

  def fork[A](a: => Par[A]): Par[A] = ???

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A]): A = ???

  // Exercise 7.1
  // Par.map2 is a new higher-order function for combining the result of two
  // parallel computations. What is its signature? Give the most general
  // signature possible (don’t assume it works only for Int).
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???
}
