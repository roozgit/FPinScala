package datastructures

import scala.annotation.tailrec

sealed trait FList[+A]
case object Nil extends FList[Nothing]
case class Cons[+A](h : A, t : FList[A]) extends FList[A]

object FList {
  def apply[A](as : A*) : FList[A] = as match {
    case x if x.isEmpty => Nil
    case _ => Cons(as.head, apply(as.tail :_*))
  }

  def size[A](as : FList[A]): Int =
    foldLeft(as, 0) ((b, _) => b + 1)

  @tailrec
  def last[A](as: FList[A]) : A = as match {
    case Cons(h, Nil) => h
    case Cons(_, t) => last(t)
    case Nil => throw new IndexOutOfBoundsException()
  }

  def append[A](as : FList[A], x : A) : FList[A] = {
    foldRight(as, Cons(x, Nil)) ((a, b) => {
      Cons(a, b)
    })
  }

  def merge[A](as1 : FList[A], as2 : FList[A]) : FList[A] = {
    foldRight(as1, as2) ((a, b) => {
      Cons(a, b)
    })
  }

  @tailrec
  def foldLeft[A, B](as : FList[A], z : B)(f : (B, A) => B) : B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldRight[A, B](as : FList[A], z : B)(f: (A, B) => B) : B =
    foldLeft(reverse(as), z)((b,a) => f(a,b))

  def reverse[A](as : FList[A]) : FList[A] =
    foldLeft(as, Nil:FList[A])((b, a) => Cons(a, b))

  def map[A, B](as : FList[A])(f: A => B): FList[B] =
    foldLeft(as, Nil : FList[B])((b, a) => Cons(f(a),b))

  def flatMap[A, B](as : FList[A])(f: A => FList[B]): FList[B] =
    foldLeft(as, Nil : FList[B])((b, a) => merge(b, f(a)))

  def hasSubsequence[A](as1 : FList[A], as2 : FList[A]) : Boolean = ???
}