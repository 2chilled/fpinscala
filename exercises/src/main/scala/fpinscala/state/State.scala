package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapUsingFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = rng.nextInt
    val j = i match {
      case Int.MinValue => 0
      case j => if (j < 0) -j else j
    }
    (j, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, nextRng) = RNG.nonNegativeInt(rng)
    val digits = i.toString.length
    ((i / digits).toDouble, nextRng)
  }

  def doubleUsingMap(rng: RNG): (Double, RNG) = {
    RNG.map(RNG.nonNegativeInt)(i => (i / i.toString.length).toDouble)(rng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nextRng) = rng.nextInt
    val (j, nextRng2) = double(nextRng)
    ((i, j), nextRng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val t = intDouble(rng)
    ((t._1.swap), t._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng)
    val (d3, rng4) = double(rng)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def helper(count: Int, rng: RNG, l: List[Int] = Nil): (List[Int], RNG) = count match {
      case 0 => (l, rng)
      case _ =>
        val (i, rng2) = rng.nextInt
        helper(count - 1, rng2, i :: l)
    }
    helper(count, rng)
  }

  def intsUsingSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    case rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
  }

  def map2UsingFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldLeft(((Nil, _)): Rand[List[A]]) {
    case (rla, ra) => map2(rla, ra)((la, a) => a :: la)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    case rng =>
      val (a, rng2) = f(rng)
      g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) {
    case i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) rng => (mod, rng) else nonNegativeLessThan(n)
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.flatMap(b => State.unit(f(a, b))))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s2) = run(s)
    f(a).run(s2)
  }
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  def unit[A, S](a: A): State[S, A] = State(s => (a, s))

  def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] = fs.foldLeft(unit[List[A], S](Nil)) {
    case (sla, sa) => sla.map2(sa)((la, a) => a :: la)
  }
}
