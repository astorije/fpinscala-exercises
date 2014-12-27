package ch3datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  // Exercise 3.1
  // What will be the result of the following match expression?
  // Result: 3 (1 + 2)
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  // Exercise 3.2
  // Implement the function tail for removing the first element of a List. Note
  // that the function takes constant time. What are the different choices you
  // could make in your implementation if the List is Nil?
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // Exercise 3.3
  // Using the same idea, implement the function setHead for replacing the first
  // element of a List with a different value.
  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  // Exercise 3.4
  // Generalize tail to the function drop, which removes the first n elements from
  // a list. Note that this function takes time proportional only to the number of
  // elements being dropped - we don't need to make a copy of the entire List.
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  // Exercise 3.5
  // Implement dropWhile, which removes elements from the List prefix as long as
  // they match a predicate.
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  // Exercise 3.6
  // Not everything works out so nicely. Implement a function, init, that returns
  // a List consisting of all but the last element of a Lit. So, given
  // List(1, 2, 3, 4), init will return List(1, 2 3). Why can't this function be
  // implemented in constant time like tail?
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  // Exercise 3.9
  // Compute the length of a list using foldRight
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  // Exercise 3.10
  // Our implementation of foldRight is not tail-recursive and will result in a
  // StackOverflowError for large lists (we say it's not stack-safe). Convince
  // yourself that this is the case, and then write another general list-recursion
  // function, foldLeft, that is tail-recursive, using the techniques we discussed
  // in the previous chapter.
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 3.11
  // Write sum, product, and a function to compute the length of a list using
  // foldLeft.
  def sumLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
  def productLeft(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)
  def lengthLeft[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  // Exercise 3.12
  // Write a function that returns the reverse of a list (given List(1, 2, 3) it
  // returns List(3, 2, 1)). See if you can write it using a fold.
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, el) => Cons(el, acc))

  // Exercise 3.13
  // Hard: Can you write foldLeft in terms of foldRight? How about the other way
  // around? Implementing foldRight via foldLeft is useful because it lets us
  // implement foldRight tail-recursively, which means it works even for large
  // lists without overflowing the stack.
  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, z)((a: A, b: B) => f(b, a))

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((b: B, a: A) => f(a, b))

  // Exercise 3.14
  // Implement append in terms of either foldLeft or foldRight.
  def appendFold[A](a1: List[A], a2: List[A]): List[A] =
    // foldLeft(reverse(a1), a2)((b: List[A], a: A) => Cons(a, b))
    foldRight(a1, a2)((a: A, b: List[A]) => Cons(a, b))

  // Exercise 3.15
  // Hard: Write a function that concatenates a list of lists into a single list.
  // Its runtime should be linear in the total length of all lists. Try to use
  // functions we have already defined.
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)
    // foldRight(l, List[A]())((el: List[A], acc: List[A]) => append(el, acc))

  // Exercise 3.16
  // Write a function that transforms a list of integers by adding 1 to each element.
  // (Reminder: this should be a pure function that returns a new List !)
  def increment(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((el: Int, acc: List[Int]) => Cons(el + 1, acc))
}
