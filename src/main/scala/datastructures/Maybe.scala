package datastructures

sealed trait Maybe[+A] { self =>
  def map[B](f : A => B) : Maybe[B] = self match {
    case Just(x) => Just(f(x))
    case None2 => None2
  }

  def flatMap[B](f: A => Maybe[B]): Maybe[B] =
    map(f) getOrElse(None2)

  def getOrElse[B >: A](default: => B): B = self match {
    case Just(x) => x
    case None2 => default
  }

  def orElse[B >: A](ob: => Maybe[B]): Maybe[B] =
    map(a => Just(a)) getOrElse ob

  def filter(f: A => Boolean): Maybe[A] =
    flatMap(a => if (f(a)) Just(a) else None2)
}

final case class Just[+T](get : T) extends Maybe[T]
case object None2 extends Maybe[Nothing]

object Maybe {
  def lift[A,B](f: A => B): Maybe[A] => Maybe[B] = _ map f

  def map2[A,B,C](a: Maybe[A], b: Maybe[B])(f: (A, B) => C): Maybe[C] =
    a.flatMap(ax => b.map(bx => f(ax, bx)))

  def sequence[A](as: FList[Maybe[A]]): Maybe[FList[A]] = {
    Just(FList.foldRight(as, Nil : FList[A])((a, b) => {
      a match {
        case None2 => return None2
        case Just(x) => Cons(x, b)
      }
    }))
  }

  def traverse[A, B](a: FList[A])(f: A => Maybe[B]): Maybe[FList[B]] =
    Just(FList.foldRight(a, Nil : FList[B]) ((x, y) => {
      f(x) match {
        case None2 => return None2
        case Just(z) => Cons(z, y)
      }
    }))

}