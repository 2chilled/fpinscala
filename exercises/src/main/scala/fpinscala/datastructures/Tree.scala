package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def fold[A,B](t:Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size[A](t: Tree[A]): Int = fold(t)(i => 1)(1 + _ + _)

  def maximum(t: Tree[Int]): Int = fold(t)(identity)(_ max _)

  def depth[A](t: Tree[A]): Int = fold(t)(i => 1)((b1, b2) => (1 + b1).max(1 + b2))

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)((a => Leaf(f(a)):Tree[B]))((b1,b2) => Branch(b1,b2))

}