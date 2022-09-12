package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E, +A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Right(a) => Right(f(a))
    case Left(b)  => Left(b)

  def getOrElse[B >: A](default: => B): B = this match
    case Right(a) => a
    case _        => default

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this.map(f) match
      case Right(get) => get
      case Left(get)  => Left(get)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this.map(a => Right(a)).getOrElse(b)

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for
      a <- this
      b <- b
    yield f(a, b)

object Either:
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    def go(as: List[A], acc: List[B]): Either[E, List[B]] =
      as match
        case x :: xs => f(x).flatMap(a => go(xs, a :: acc))
        case _       => Right(acc.reverse)
    go(es, Nil)

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(a => a)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if xs.isEmpty then Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](
      a: Either[List[E], A],
      b: Either[List[E], B],
      f: (A, B) => C
  ): Either[List[E], C] = (a, b) match
    case (Right(aa), Right(bb)) => Right(f(aa, bb))
    case (Left(aa), Right(_))   => Left(aa)
    case (Right(_), Left(bb))   => Left(bb)
    case (Left(aa), Left(bb))   => Left(aa ++ bb)

  def traverseAll[E, A, B](
      es: List[A],
      f: A => Either[List[E], B]
  ): Either[List[E], List[B]] =
    def go(as: List[A], acc: List[B], ers: List[E]): Either[List[E], List[B]] =
      as match
        case x :: xs =>
          f(x) match
            case Right(y) => go(xs, y :: acc, ers)
            case Left(y)  => go(xs, acc, y ++ ers)
        case Nil =>
          ers match
            case Nil => Right(acc.reverse)
            case _   => Left(ers.reverse)
    go(es, Nil, Nil)

  def sequenceAll[E, A](
      es: List[Either[List[E], A]]
  ): Either[List[E], List[A]] = traverseAll(es, a => a)
