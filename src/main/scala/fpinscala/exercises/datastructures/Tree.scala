package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = 
    this match
      case Branch(l,r) => (l.depth).max(r.depth) + 1
      case Leaf(_) => 0

  def map[B](f: A => B): Tree[B] = 
    this match
      case Leaf(i) => Leaf(f(i))
      case Branch(l,r) => Branch(l.map(f), r.map(f))
    

  def fold[B](f: A => B, g: (B,B) => B): B = 
    this match
      case Leaf(i) => f(i)
      case Branch(l, r) => g(l.fold(f,g), r.fold(f,g))
  
  def sizeViaFold: Int = this.fold(a => 1, 1+_ + _)
  
  def depthViaFold: Int = this.fold(a=>0, (a:Int,b:Int) => a.max(b)+1)
  
  def mapViaFold[B](f: A => B): Tree[B] = this.fold(a => Leaf(f(a)), (l,r) => Branch(l,r))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Option[Int] =
    t match
      case Leaf(i) => if i>0 then Some(i) else None
      case Branch(l, r) => l.firstPositive orElse r.firstPositive

  extension (t: Tree[Int]) def maximum: Int = 
    t match
      case Leaf(i) => i
      case Branch(l,r) => (l.maximum).max(r.maximum)

  extension (t: Tree[Int]) def maximumViaFold: Int = t.fold(a => a, (a:Int,b:Int) => a.max(b))
