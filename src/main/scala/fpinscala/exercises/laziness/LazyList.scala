package fpinscala.exercises.laziness

import fpinscala.answers.streamingio.SimplePulls.Stream.fold
import scala.annotation.TypeConstraint

import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = foldRight(Nil: List[A])(_ :: _)

  def foldRight[B](
      z: => B
  )(
      f: (A, => B) => B
  ): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h, t) =>
        f(
          h(),
          t().foldRight(z)(f)
        ) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) =>
      p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] =
    this match
      case Cons(h, t) if n > 0 => Cons(() => h(), () => t().take(n - 1))
      case _                   => Empty

  def drop(n: Int): LazyList[A] =
    this match
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case _                   => this

  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(Empty: LazyList[A])((a, b) =>
      if p(a) then Cons(() => a, () => b) else b
    )

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) =>
      p(a) && b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def headOption: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  def map[B](f: A => B): LazyList[B] =
    foldRight(Empty: LazyList[B])((a, b) => Cons(() => f(a), () => b))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(Empty: LazyList[A])((a, b) =>
      if p(a) then Cons(() => a, () => b) else b
    )

  def append[A2 >: A](ls: => LazyList[A2]): LazyList[A2] =
    foldRight(ls)((a, b) => Cons(() => a, () => b))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _          => None
    }

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1)          => Some(h(), (empty, 0))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _                        => None
    }

  def takeWhileViaUnfold(f: A => Boolean): LazyList[A] =
    unfold(this) {
      case (Cons(h, t)) if f(h()) => Some((h(), t()))
      case _                      => None
    }

  def zipWith[B, C](other: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, other)) {
      case (Cons(t, ts), Cons(o, os)) => Some((f(t(), o()), (ts(), os())))
      case _                          => None
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Cons(t, ts), Cons(o, os)) =>
        Some(((Some(t()), Some(o())), (ts(), os())))
      case (Cons(t, ts), Empty) => Some(((Some(t()), None), (ts(), Empty)))
      case (Empty, Cons(o, os)) => Some(((None, Some(o())), (Empty, os())))
      case _                    => None
    }

  def startsWith[B](s: LazyList[B]): Boolean =
    zipAll(s).takeWhile(_(1).isDefined).forAll { case (x, y) => x == y }

  def tails: LazyList[LazyList[A]] =
    unfold(this) {
      case x @ Cons(a, as) => Some((x, as()))
      case _               => None
    }.append(LazyList(empty[A]))

  def hasSubsequence[B >: A](sub: LazyList[B]): Boolean =
    this.tails.exists(_.startsWith(sub))

  def scanRight[B](start: B)(f: (A, => B) => B): LazyList[B] =
    foldRight(LazyList(start)) { (a, b) =>
      lazy val b1 = b
      cons(f(a, b1.headOption.get), b1)
    }

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    lazy val single: LazyList[A] = cons(a, single)
    single

  def from(n: Int): LazyList[Int] =
    lazy val single: LazyList[Int] = cons(n, single.map(_ + 1))
    single

  def zipWith[A, B, C](as: => LazyList[A], bs: => LazyList[B])(
      f: (=> A, => B) => C
  ): LazyList[C] =
    (as, bs) match
      case (Cons(x, xs), Cons(y, ys)) =>
        cons(f(x(), y()), zipWith(xs(), ys())(f))
      case _ => Empty

  lazy val fibs: LazyList[Int] =
    cons(
      0,
      cons(
        1,
        zipWith(fibs, fibs.drop(1))(_ + _)
      )
    )

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _            => empty[A]

  lazy val fibsViaUnfold: LazyList[Int] =
    type State = (Int, Int)
    def go(state: State): Option[(Int, State)] =
      Some((state._1, (state._2, state._1 + state._2)))
    unfold((0, 1))(go)

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(s => Some((s, s + 1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(())((_) => Some((a, ())))

  lazy val onesViaUnfold: LazyList[Int] =
    unfold(())((_) => Some((1, ())))
