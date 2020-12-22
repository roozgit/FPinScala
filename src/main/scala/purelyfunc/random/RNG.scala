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

object RNG {
  def map[A, B](s : Rand[A])(f : A => B) : Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def map2[A, B, C](ra : Rand[A])(rb : Rand[B])(f : (A, B) => C) : Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def both[A, B](ra : Rand[A], rb : Rand[B]) : Rand[(A, B)] =
    map2(ra)(rb)((_, _))

  def sequence[A](fs : List[Rand[A]]) : Rand[List[A]] = {
    rng => {
      fs.foldLeft((List.empty[A], rng)) ( (a, b) => {
        val (a1, nrng) = b(a._2)
        (a._1 :+ a1, nrng)
      })
    }
  }

  def flatMap[A, B](f : Rand[A])(g : A => Rand[B]) : Rand[B] = ???

  def int(rng : RNG) : (Int, RNG) =
    rng.nextInt

  def double(rng : RNG) : (Double, RNG) =
    map(_.nextInt)(x => math.abs(x.toDouble) / Int.MaxValue)(rng)

  def intDouble(rng : RNG) : ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def ints(count : Int)(rng : RNG) : (List[Int], RNG) = {
    val lst = List.fill(count)(1)
    val l2 : List[Rand[Int]] = lst.map(x => _.nextInt)

    sequence(l2)(rng)
  }

  def nonNegativeInt(rng : RNG) : (Int, RNG) = {
    val (i , rng2) = rng.nextInt
    if(i == Int.MinValue)
      (Int.MaxValue, rng2)
    else (math.abs(i), rng2)
  }


  def nonNegativeLessThan(n : Int) : Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1)  - mod >= 0)
      (mod , rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)

    println(nonNegativeLessThan(70)(rng))


  }
}