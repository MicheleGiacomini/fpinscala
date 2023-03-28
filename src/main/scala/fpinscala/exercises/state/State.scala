package fpinscala.exercises.state

import RNG.*

trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(
        newSeed
      ) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (
        n,
        nextRNG
      ) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val next = rng.nextInt
    next match
      case (n, rng2) if n < 0 => (-(n + 1), rng2)
      case _                  => next

  def double(rng: RNG): (Double, RNG) =
    val (i, rng2) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue.toDouble), rng2)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    count match
      case c if c == 0 => (Nil: List[Int], rng)
      case _ =>
        val (i, r1) = rng.nextInt
        val (is, r2) = ints(count - 1)(r1)
        (i :: is, r2)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rand => {
      val (a, r1) = ra(rand)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = rs.foldRight(unit(Nil:List[A]))((a,b)=>map2(a,b)(_::_))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a,r1) = r(rng)
      val rb = f(a)
      rb(r1)
    }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C
  ): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a,b)))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])

    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      s => {
        val (a,s1) = run(s)
        (f(a),s1)
      }

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      s => {
        val (a,s1) = run(s)
        val (b,s2) = sb.run(s1)
        (f(a,b), s2)
      }

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      }

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S,A](a: A): State[S,A] =
    s => (a,s)

  def sequence[S,A](rs: List[State[S,A]]): State[S,List[A]] = rs.foldRight(unit[S,List[A]](Nil:List[A]))((a,b)=>a.map2(b)(_::_))

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
