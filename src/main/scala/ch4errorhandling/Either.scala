package ch4errorhandling

sealed trait Either[+E, +A] {

  // Exercise 4.6
  // Implement versions of map, flatMap, orElse, and map2 on Either that operate
  // on the Right value.
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(a => b.map(bb => f(a, bb)))
    // for {
    //   a <- this
    //   b1 <- b
    // } yield f(a,b1)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  // Exercise 4.7
  // Implement sequence and traverse for Either. These should return the first
  // error that’s encountered, if there is one.
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight[Either[E, List[A]]](Right(Nil))(
      (e: Either[E, A], acc: Either[E, List[A]]) => e.map2(acc)(_ :: _)
    )

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))(
      (a: A, acc: Either[E, List[B]]) => f(a).map2(acc)(_ :: _)
    )
}
