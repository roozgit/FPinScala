package datastructures

import scala.annotation.tailrec

import LStream._
sealed trait LStream[+A] { self =>

  def toListRecursive: List[A] = this match {
    case LCons(h,t) => h() :: t().toListRecursive
    case Empty => List()
  }

  private def toList: List[A] = {

    @tailrec def innerToList(x: List[A], s: LStream[A]): List[A] = s match {
      case LCons(hf, tf) => innerToList(hf() :: x, tf())
      case Empty => x
    }
    innerToList(List.empty[A], self).reverse
  }

  def drop(n : Int) : LStream[A] = {
    @tailrec def innerDrop(k : Int, s : LStream[A]) : LStream[A] = (k, s) match {
      case (0, x) => x
      case (p, LCons(_, tf)) => innerDrop(p  - 1, tf())
    }

    innerDrop(n , self)
  }

  def take(n : Int) : LStream[A] = {
    def innerTake(k : Int, s : LStream[A]) : LStream[A] = (k, s) match {
      case (_, Empty) => s
      case (1, LCons(x, y)) => LCons(x, () => Empty)
      case (p, LCons(hf, tf)) => LCons(hf, () => innerTake(p - 1, tf()))
    }

    innerTake(n , self)
  }

  def takeWhile(p : A => Boolean) : LStream[A] = self match {
      case Empty => self
      case LCons(hf, tf) if p(hf()) => LCons(hf, () => tf().takeWhile(p))
      case LCons(hf, _) if !p(hf()) => Empty
    }

  def exists(p : A => Boolean): Boolean = self match {
    case LCons(hf, tf) => p(hf()) || tf().exists(p)
    case _ => false
  }

  def forall(p : A => Boolean) :Boolean = self match {
      case LCons(hf, tf) => p(hf()) && tf().forall(p)
      case _ => true
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = self match {
      case LCons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def takeWhileWithFoldRight(p : A => Boolean) : LStream[A] =
    self.foldRight(empty[A]) ( (a, b) => if(p(a)) cons(a, b) else empty)

  def headOption() : Option[A] = self match {
    case Empty => None
    case LCons(hf, tf) => Some(hf())
  }

  def headOptionWithFoldRight() : Option[A] =
    self.foldRight(None : Option[A]) ((a, _) => Some(a))

  def map[B](f : A => B) : LStream[B] =
    self.foldRight(empty[B]) ((a, b) => cons(f(a), b))

  def filter(p : A => Boolean) : LStream[A] =
    self.foldRight(empty[A])((a, b) => if(p(a)) cons(a,b) else b)

  def append[B >: A](y : => B) : LStream[B] =
    self.foldRight(cons(y, empty[B]))((a, b) => cons(a,b))

  def startsWith[B >: A] (s: LStream[B]): Boolean = (self, s) match {
    case (LCons(ahf, atf), LCons(bhf, btf)) if ahf() == bhf() => atf().startsWith(btf())
    case (LCons(_, _), Empty) => true
    case (Empty, Empty) => true
    case _ => false
  }

  def tails() : LStream[LStream[A]] =
    unfold(self) {
      case LCons(hf, tf) => Some(LCons(hf, tf), tf())
      case _ => None
    }

  def hasSubSequence[B >: A](s : LStream[B]) : Boolean =
    tails exists (_ startsWith s)
}

case object Empty extends LStream[Nothing]
case class LCons[+A](h: () => A, t: () => LStream[A]) extends LStream[A]

object LStream {
  def cons[A](hd: => A, tl: => LStream[A]): LStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    LCons(() => head, () => tail)
  }

  def empty[A]: LStream[A] = Empty

  def apply[A](as: A*): LStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): LStream[A] = {
    lazy val cs : LStream[A] = cons(a, cs)
    cs
  }

  def from(n: Int): LStream[Int] = {
    lazy val s : LStream[Int] = cons(n, from(n + 1))
    s
  }

  lazy val fibo : LStream[Int] = {
    def fibs(a:  Int , b : Int) : LStream[Int] = {
      lazy val fx = cons(a, fibs(b, a + b))
      fx
    }
    fibs(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): LStream[A] =
    f(z) match {
      case None => empty[A]
      case Some((a, ns)) => cons(a, unfold(ns)(f))
    }

  def fromWithUnfold(n : Int) : LStream[Int] =
    unfold(n)( s => Some(s, s + 1))

  def constantWithUnfold[A](c : A) : LStream[A] =
    unfold(c)( s => Some(c, c))

  def fibsWithUnfold(a: Int , b: Int) : LStream[Int] =
    unfold((a, b))( s => Some(s._1, (s._2, s._1 + s._2)))

  def mapWithUnfold[A, B](als: LStream[A])(f : A => B) : LStream[B] =
    unfold(als)(s => s.headOption().map(sx => (f(sx), s.drop(1))))

  def takeWithUnfold[A, B](als: LStream[A])(n : Int) : LStream[A] =
    unfold((n, als)) {
      case (x, t) if x == 0 => None
      case (x, Empty) => None //allows for take(n > number_of_elements)
      case (x, LCons(hf, tf)) => Some((hf(), (x - 1, tf())))
    }

  def takeWhileWithUnfold[A, B](als : LStream[A])(p : A => Boolean) : LStream[A] =
    unfold(als)(s => s.headOption().filter(p).map(sr => (sr, s.drop(1))))

  def zipWith[A, B, C](als : LStream[A], bls : LStream[B])(f : (A, B) => C): LStream[C] =
    unfold((als, bls)) {
      case (LCons(ahf, atf), LCons(bhf, btf)) => Some(f(ahf(),bhf()), (atf(), btf()))
      case _ => None
    }

  //MAIN
  def main(args: Array[String]): Unit = {
    val lst = LStream(1, 2, 3, 4, 5)
    val lst2 = LStream(3, 4, 1)
    val r = lst.hasSubSequence(lst2)
    println(r)



  }
}