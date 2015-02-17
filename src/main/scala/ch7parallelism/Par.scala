package ch7parallelism

import java.util.concurrent._

object Par {
  // Exercise 7.2
  // Before continuing, try to come up with representations for Par that make it
  // possible to implement the functions of our API.
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] = ???

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // Exercise 7.1
  // Par.map2 is a new higher-order function for combining the result of two
  // parallel computations. What is its signature? Give the most general
  // signature possible (don’t assume it works only for Int).
  //
  // This implementation of map2 does not respect timeouts. It simply passes the
  // ExecutorService on to both Par values, waits for the results of the Futures
  // af and bf, applies f to them, and wraps them in a UnitFuture. In order to
  // respect timeouts, we’d need a new Future implementation that records the
  // amount of time spent evaluating af, and then subtracts that time from the
  // available time allocated for evaluating bf.
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  // Exercise 7.3
  // Hard: Fix the implementation of map2 so that it respects the contract of
  // timeouts on Future.
  private case class Map2Future[A, B, C](fa: Future[A], fb: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None
    def isDone = cache.isDefined
    def get(timeout: Long, units: TimeUnit) = compute(TimeUnit.MILLISECONDS.convert(timeout, units))
    def get = compute(Long.MaxValue)
    def isCancelled = fa.isCancelled || fb.isCancelled
    def cancel(evenIfRunning: Boolean): Boolean = fa.cancel(evenIfRunning) || fb.cancel(evenIfRunning)

    private def compute(timeoutMs: Long): C = cache match {
      case Some(c) => c
      case None => {
        val start = System.currentTimeMillis
        val ra = fa.get(timeoutMs, TimeUnit.MILLISECONDS)
        val delta = System.currentTimeMillis - start
        val rb = fb.get(timeoutMs - delta, TimeUnit.MILLISECONDS)
        val r = f(ra, rb)
        cache = Some(r)
        r
      }
    }
  }

  def timeoutMap2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

  // Exercise 7.4
  // This API already enables a rich set of operations. Here’s a simple example:
  // using lazyUnit, write a function to convert any function A => B to one that
  // evaluates its result asynchronously.
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
}
