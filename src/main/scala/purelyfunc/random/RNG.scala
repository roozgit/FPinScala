package purelyfunc.random

import purelyfunc.Rand

trait RNG {
  def nextInt : (Int, RNG)
}

case class SimpleRNG(seed : Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed  = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

import State._
case class State[S, +A](run : S => (A, S)) {

  def map[B](f : A => B) : State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })

  def get : State[S, S] = State(s => (s, s))

  def set(s : S) : State[S, Unit] = State(_ => ((), s))

  def modify(f : S => S) : State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

object State {
  def unit[S, A](a: A) : State[S, A] =
    State(s => (a, s))

  def sequence[S, A](fs : List[State[S, A]]) : State[S, List[A]] =
    State(s => {
      fs.foldLeft((List.empty[A], s)) ((a, b) => {
        val (ap, sp) = b.run(a._2)
        (a._1 :+ ap, sp)
      })
    })
}

object RNG {
  def int : Rand[Int] =
    State(rng => rng.nextInt)

  def nonNegativeInt : Rand[Int] =
    int.flatMap(i => State(rng => {
      if(i == Int.MinValue)
        (Int.MaxValue, rng)
      else
        (math.abs(i), rng)
    }))

  def nonNegativeLessThan(n : Int) : Rand[Int] =
    nonNegativeInt.flatMap(i => {
      val mod = i % n
      if(i + (n - 1) - mod >= 0)
        State.unit(mod)
      else
        nonNegativeLessThan(n)
    })

  def ints(count : Int) : Rand[List[Int]] = {
    val lst = List.fill(count)(State((rng : RNG) => rng.nextInt))
    State.sequence(lst)
  }

  def nonNegativeIntsLessThan(count : Int, n : Int) : Rand[List[Int]] = {
    val lst = List.fill(count)(nonNegativeLessThan(n))
    State.sequence(lst)
  }

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)
    val ns : Rand[List[Int]] =
      for {
        x <- nonNegativeLessThan(9)
        y <- nonNegativeLessThan(11)
        xs <- nonNegativeIntsLessThan(x, y)
      } yield xs//.map(_ % y)
    println(ns.run(rng))
  }
}