package fpinscala.exercises.datastructures

import scala.annotation.tailrec

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

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y //This one
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

  def tail[A](l: List[A]): List[A] = l match
    case Nil => sys.error("List is empty")
    case Cons(_, tail) => tail

  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil => sys.error("List is empty")
    case Cons(_, tail) => Cons(h, tail)

  @tailrec def drop[A](l: List[A], n: Int): List[A] = l match
    case Nil => Nil
    case Cons(_, t) => if n <= 0 then l else drop(t, n - 1)

  @tailrec def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l

  def init[A](l: List[A]): List[A] = l match
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))

  def length[A](l: List[A]): Int = foldRight(l, 0, (_, length) => length + 1)

  @tailrec def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil => acc
    case Cons(h, t) => foldLeft(t, f(acc, h), f)

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, Nil, (l2, e) => Cons(e, l2))

  def foldLeftViaFoldLeft[A,B](as: List[A], acc: B, f: (A, B) => B): B = foldLeft(reverse(as), acc, (b, a) => f(a, b))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r, (elem, list) => Cons(elem, list))

  def concat[A](l: List[List[A]]): List[A] = foldRight[List[A], List[A]](l, Nil, append)

  def incrementEach(l: List[Int]): List[Int] = foldRight[Int, List[Int]](l, Nil, (elem, list) => Cons(elem + 1, list))

  def doubleToString(l: List[Double]): List[String] = foldRight[Double, List[String]](l, Nil, (elem, list) => Cons(elem.toString, list))

  def map[A,B](l: List[A], f: A => B): List[B] = foldRight[A, List[B]](l, Nil, (elem, list) => Cons(f(elem), list))

  def filter[A](as: List[A], f: A => Boolean): List[A] = foldRight[A, List[A]](as, Nil, (elem, list) => if f(elem) then Cons(elem, list) else list)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] = foldRight[A, List[B]](as, Nil, (elem, list) => append(f(elem), list))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = flatMap(as, elem => if f(elem) then Cons(elem, Nil) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h, t), Cons(h2, t2)) => Cons(h + h2, addPairwise(t, t2))

  def zipWith[A, B](a: List[A], b: List[A], f: (A, A) => B): List[B] = (a, b) match
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h, t), Cons(h2, t2)) => Cons(f(h, h2), zipWith(t, t2, f))

  @tailrec def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match
    case Nil => sub == Nil
    case _ if startWith(sup, sub) => true
    case Cons(head, tail) => hasSubsequence(tail, sub)

  @tailrec private def startWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startWith(t1, t2)
    case _ => false
