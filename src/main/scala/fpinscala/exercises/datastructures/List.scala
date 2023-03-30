package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil

  /** Another data constructor, representing nonempty lists. Note that `tail` is
    * another `List[A]`, which may be `Nil` or another `Cons`.
    */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(
          xs
        ) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  def foldRight[A, B](
      as: List[A],
      acc: B,
      f: (A, B) => B
  ): B = // Utility functions
    as match
      case Nil         => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(
      ns,
      1.0,
      _ * _
    ) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match
    case Nil         => sys.error("Tail of empty list")
    case Cons(_, ls) => ls

  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil         => sys.error("Tail of empty list")
    case Cons(_, ls) => Cons(h, ls)

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match
    case (Nil, _)         => Nil
    case (l, n) if n <= 0 => l
    case (l, n)           => drop(tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Nil                 => Nil
    case Cons(l, ls) if f(l) => dropWhile(ls, f)
    case _                   => l

  def init[A](l: List[A]): List[A] = l match
    case Nil          => sys.error("Init of empty list")
    case Cons(h, Nil) => Nil
    case Cons(h, hs)  => Cons(h, init(hs))

  def length[A](l: List[A]): Int = foldRight(l, 0, (_, b) => 1 + b)

  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil         => acc
    case Cons(h, ls) => foldLeft(ls, f(acc, h), f)

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (b, _) => b + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A], (b, a) => Cons(a, b))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil: List[A], appendViaFoldRight(_, _))

  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int], (a, b) => Cons(a + 1, b))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String], (a, b) => Cons(a.toString(), b))

  def map[A, B](l: List[A], f: A => B): List[B] =
    foldRight(l, Nil: List[B], (a, b) => Cons(f(a), b))

  def filter[A](as: List[A], f: A => Boolean): List[A] = as match
    case Nil                 => Nil
    case Cons(a, as) if f(a) => Cons(a, filter(as, f))
    case Cons(_, as)         => filter(as, f)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] = concat(map(as, f))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = zipWith(a, b, _ + _)

  // def zipWith - TODO determine signature
  def zipWith[A, B, C](as: List[A], bs: List[B], f: (A, B) => C): List[C] =
    (as, bs) match
      case (Nil, _)                   => Nil
      case (_, Nil)                   => Nil
      case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs, f))

  def forAll[A](a: List[A], f: A => Boolean): Boolean =
    foldLeft(a, true, (b, a) => b & f(a))

  def allTrue(a: List[Boolean]): Boolean =
    forAll(a, identity)

  def startsWith[A](sup: List[A], sub: List[A]): Boolean =
    allTrue(zipWith(sup, sub, _ == _))

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match
    case Nil                       => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, hs)               => hasSubsequence(hs, sub)
