package datastructures

import scala.annotation.tailrec

sealed trait RTree[A]
final case class Leaf[A](x: A) extends RTree[A]
final case class Branch[A](xs: RTree[(A,A)]) extends RTree[A]

object RTree {
  @tailrec def foldLeft[A,R](t: RTree[A])(init: R)(f: (R,A)=>R): R = t match {
    case Leaf(y) => f(init, y)
    case Branch(xs) => foldLeft(xs)(init) {
      case (r, (left, right)) => f(f(r, left), right)
    }
  }

  def main(args: Array[String]): Unit = {
    val t = foldLeft(Branch(Branch(Branch(Leaf(((1,2),(3,4)),((5,6),(7,8)))))))(0)(_+ _)
    val s = foldLeft(Branch(Branch(Leaf((("a","b"),("c","d"))))))("")(_ + _)
    println(t)
    println(s)
  }
}