package fpinscala.datastructures

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    if (a1 != Nil)
      foldRight(a1, Nil: List[A]) {
        case (a, Nil) => Cons(a, a2)
        case (a, l) => Cons(a, l)
      }
    else a2

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = drop(l, 1)

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case l@Cons(_, t) => if (n == 0) l else drop(t, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case l2@Cons(h, t) => if (f(h)) dropWhile(t, f) else l2
  }

  def init[A](l: List[A]): List[A] = {
    def helper(original: List[A]): List[A] = original match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, helper(t))
    }
    helper(l)
  }

  def length[A](l: List[A]): Int = foldLeft(l, 0) {
    case (i, _) => i + 1
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight[A, List[B]](l, Nil) {
    case (a, l2) => Cons(f(a), l2)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((l, a) => Cons(a, l))

  def join[A](l: List[List[A]]): List[A] = foldLeft(l, Nil: List[A])(append(_, _))

  def add1(l: List[Int]): List[Int] = map(l)(_ + 1)

  def doubleToString(l: List[Double]): List[String] = map(l)(_.toString)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = join(map(as)(f))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addTogether(as1: List[Int], as2: List[Int]): List[Int] = zipWith(as1, as2)(_ + _)

  def zipWith[A](as1: List[A], as2: List[A])(f: (A, A) => A): List[A] = (as1, as2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a1, t1), Cons(a2, t2)) => Cons(f(a1, a2), zipWith(t1, t2)(f))
  }
}