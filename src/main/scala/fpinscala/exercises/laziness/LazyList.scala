package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.{cons, empty, unfold}

import scala.collection.immutable

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty

  def takeViaUnfold(n: Int): LazyList[A] = unfold((this, n)):
    case (Cons(h, t), 1) => Some(h(), (empty, 0))
    case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n -1))
    case _ => None

  def drop(n: Int): LazyList[A] = this match
    case Empty => Empty
    case Cons(h, t) if n > 0 => t().drop(n -1)
    case Cons(h, t) => Cons(h, t)

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] = unfold(this):
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None

  def takeWhile2(p: A => Boolean): LazyList[A] = foldRight(empty)((a, b) => if p(a) then cons(a, b) else b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): LazyList[B] = foldRight(empty)((elem, list) => cons(f(elem), list))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this):
      case Cons(h, t) => Some(f(h()), t())
      case _ => None

  def filter(f: A => Boolean): LazyList[A] = foldRight(empty)((elem, list) => if f(elem) then cons(elem, list) else list)

  def append[A2 >: A](list: => LazyList[A2]): LazyList[A2] = foldRight(list)(cons(_,_))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] = foldRight(empty)((elem, list) => f(elem).append(list))

  def startsWith[B](s: LazyList[B]): Boolean = ???

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
    lazy val infinite: LazyList[A] = cons(a, infinite)
    infinite

  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    def doFib(n: Int, n1: Int): LazyList[Int] =
      cons(n, doFib(n1, n + n1))
    doFib(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(state) match
    case Some((head, tail)) => cons(head, unfold(tail)(f))
    case None => empty

  lazy val fibsViaUnfold: LazyList[Int] = unfold((0, 1)):
    case (c, n) => Some((c, (n, c + n)))

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(n => Some((n, n + 1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(())(_ => Some(a, ()))

  lazy val onesViaUnfold: LazyList[Int] = unfold(())(_ => Some(1, ()))
