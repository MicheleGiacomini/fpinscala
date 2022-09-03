package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val x = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = 
    l match
      case Cons(x, xs) => xs
      case Nil => throw new Exception("Cannot take tail of empy list")

  def setHead[A](l: List[A], h: A): List[A] = 
    l match
      case Cons(x, xs) => Cons(h, xs)
      case Nil => throw new Exception("Cannot take tail of empy list")

  def drop[A](l: List[A], n: Int): List[A] = 
    (l,n) match
      case (Nil,_) => Nil
      case (_,m) if m<=0 => l
      case _ => drop(tail(l), n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match
      case Nil => Nil
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l

  def init[A](l: List[A]): List[A] = 
    l match
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
      case Nil => throw new Exception("Cannot take init of empty list")
    

  def length[A](l: List[A]): Int = 
    def go[A](l: List[A], n:Int):Int =
      l match
        case Nil => n
        case Cons(x, xs) => go(xs, n+1)
    go(l, 0)
      

  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(acc,x),f)

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, _+_)

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1, _*_)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (b,a)=>b+1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A], (b,a) => Cons(a,b))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l,r,Cons(_,_))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l,Nil:List[A],append)

  def incrementEach(l: List[Int]): List[Int] = foldRight(l,Nil,(a:Int,b:List[Int])=>Cons(a+1,b))

  def doubleToString(l: List[Double]): List[String] = foldRight(l,Nil,(a:Double, b:List[String])=>Cons(a.toString, b))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B], (a,b)=>Cons(f(a),b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = 
    as match
      case Nil => Nil
      case Cons(x,xs) if f(x) => Cons(x, filter(xs)(f))
      case _ => filter(tail(as))(f)
    

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = 
    def go(a:A):List[A] =
      if f(a) then List( a )
      else Nil:List[A]
    flatMap(as)(go)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = zipWith(a,b)(_+_)

  // def zipWith - TODO determine signature
  def zipWith[A,B,C](a:List[A], b:List[B])(f:(A,B)=>C): List[C] =
    (a,b) match
      case (Nil,_) => Nil
      case (_, Nil) => Nil
      case (Cons(x,xs), Cons(y,ys)) => Cons(f(x,y),zipWith(xs,ys)(f))
    

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = 
    (sup, sub) match
      case (_,Nil) => true
      case (Nil, _) => false
      case (Cons(x,xs),Cons(y,ys)) if x==y => hasSubsequence(xs,ys)
      case _ => hasSubsequence(tail(sup),sub)

    
