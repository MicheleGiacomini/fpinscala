// package fpinscala.exercises.parallelism
// import fpinscala.exercises.parallelism.Par

// // First iteration not parallelizable
// def sum(ints: Seq[Int]): Int =
//   ints.foldLeft(0)((a, b) => a + b)

// //Second try, this can be parallelized
// //The last line suggests the Par type
// //with a unit and a get functions
// def sum1(ints: IndexedSeq[Int]): Int =
//   if ints.size <= 1 then ints.headOption.getOrElse(0)
//   else
//     val (l, r) = ints.splitAt(ints.size / 2)
//     sum1(l) + sum1(r)

// //Try using Par, unit and get to parallelize
// //the computation
// def sum2(ints: IndexedSeq[Int]): Int =
//   if ints.size <= 1 then ints.headOption.getOrElse(0)
//   else
//     val (l, r) = ints.splitAt(ints.size / 2)
//     val sumL: Par[Int] = Par.unit(sum2(l))
//     val sumR: Par[Int] = Par.unit(sum2(r))
//     Par.get(sumL) + Par.get(
//       sumR
//     ) // unit will need to start evaluating the argument straight away,
//     // otherwise this last line makes the computation effectively sequential
//     // In Scala function arguments are strictly evaluated left to right!
// // NOTE: if unit starts evaluating the arguments straight away we won't haave referential transparency
// // anymore. Indeed inlining sumL and sumR we get a sequential computation because of the note above

// //A possible solution to this problem is to (again) delay the execution of the program:
// //make the function return a Par[Int] -unevaluated- instead of an Int.
// def sum3(ints: IndexedSeq[Int]): Par[Int] =
//   if ints.size <= 1 then Par.unit(ints.headOption.getOrElse(0))
//   else
//     val (l, r) = ints.splitAt(ints.size / 2)
//     Par.map2(sum3(l), sum3(r))(_ + _)

// class Par[A]

// object Par:

//   def unit[A](a: A): Par[A] = ???

//   def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

//   def get[A](a: Par[A]): A = ???

//   def map2[A, B, C](r: Par[B], l: Par[C])(f: (B, C) => A): Par[A] = ???

//   // Introduce fork to avoid having map2 be too strict and evaluate one side before the other, but only
//   // if the programmer wishes to do so.
//   def fork[A](a: => Par[A]): Par[A] = ???
