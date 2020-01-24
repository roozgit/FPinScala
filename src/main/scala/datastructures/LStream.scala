package datastructures

import scala.annotation.tailrec

sealed trait LStream[+A] { self =>

  def toListRecursive: List[A] = this match {
    case LCons(h,t) => h() :: t().toListRecursive
    case Empty => List()
  }

  def toList: List[A] = {

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
}