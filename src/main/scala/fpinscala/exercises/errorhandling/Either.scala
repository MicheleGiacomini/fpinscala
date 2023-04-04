package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E, +A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Right(a) => Right(f(a))
    case Left(e)  => Left(e)

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this.map(f) match
      case Right(Right(b)) => Right(b)
      case Right(Left(ee)) => Left(ee)
      case Left(e)         => Left(e)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Right(a) => Right(a)
    case _        => b

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(a => b.map(b => f(a, b)))

object Either:
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    @annotation.tailrec
    def go(es: List[A], acc: List[B]): Either[E, List[B]] =
      es match
        case e :: es =>
          f(e) match
            case Right(get) => go(es, get :: acc)
            case Left(get)  => Left(get)
        case Nil => Right(acc.reverse)
    go(es, Nil)

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

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
    case (Right(a), Right(b))   => Right(f(a, b))
    case (Left(es1), Left(es2)) => Left(es1.concat(es2))
    case (Left(es1), _)         => Left(es1)
    case (_, Left(es2))         => Left(es2)

  def traverseAll[E, A, B](
      as: List[A],
      f: A => Either[List[E], B]
  ): Either[List[E], List[B]] =
    as.foldRight(Right(Nil): Either[List[E], List[B]])((a, b) =>
      map2All(f(a), b, _ :: _)
    )

  def sequenceAll[E, A](
      as: List[Either[List[E], A]]
  ): Either[List[E], List[A]] = traverseAll(as, identity)
