package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_)      => 0
    case Branch(l, r) => l.depth.max(r.depth) + 1

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(a)      => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Leaf(a)             => f(a)
    case Branch(left, right) => g(left.fold(f, g), right.fold(f, g))

  def sizeViaFold: Int = this.fold(_ => 1, _ + _ + 1)

  def depthViaFold: Int = this.fold(_ => 0, _.max(_) + 1)

  def mapViaFold[B](f: A => B): Tree[B] =
    this.fold(a => Leaf(f(a)), (l, r) => Branch(l, r))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  extension (t: Tree[Int])
    def firstPositive: Int =
      def compare(l: Int, r: Int): Int = if l > 0 then l else r
      t.fold(identity, compare)

  extension (t: Tree[Int])
    def maximum: Int = t match
      case Leaf(t)      => t
      case Branch(l, r) => l.maximum.max((r.maximum))

  extension (t: Tree[Int]) def maximumViaFold: Int = t.fold(identity, _.max(_))
