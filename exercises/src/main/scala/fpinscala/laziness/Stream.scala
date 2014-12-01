package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = {
    def helper(n: Int, source: Stream[A]): Stream[A] = {
      if (n == 0) empty
      else
        source match {
          case Empty => empty
          case Cons(h, t) => cons(h(), helper(n - 1, t()))
        }
    }
    helper(n, this)
  }

  def drop(n: Int): Stream[A] = sys.error("todo")

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A]) {
    case (a, s) => if (p(a)) cons(a, s) else s
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)(p(_) && _)

  def startsWith[B](s: Stream[B]): Boolean = {
    @tailrec
    def helper(s1: Stream[A], s2: Stream[B]): Boolean = (s1, s2) match {
      case (Empty, Cons(_, _)) => false
      case (_, Empty) => true
      case (Cons(a, t), Cons(a2, t2)) => if (a() == a2()) helper(t(), t2()) else false
    }

    this match {
      case Empty => s == Empty
      case _ if (s == Empty) => false
      case _ => helper(this, s)
    }
  }

  def toList: List[A] = foldRight(Nil: List[A])(_ :: _)

  def headOption: Option[A] = foldRight(None: Option[A]) {
    case (a, _) => Some(a)
  }
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
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = unfold(n)(i => if (i > 0) Some(n, i - 1) else None)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    @tailrec
    def helper(z: S, target: Stream[A] = empty): Stream[A] = f(z) match {
      case Some((a, s)) => helper(s, cons(a, target))
      case None => target
    }
    helper(z)
  }
}