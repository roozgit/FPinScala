package purelyfunc

trait RNG {
  def nextInt : (Int, RNG)
  def nextDouble : (Double, RNG)
}

case class SimpleRNG(seed : Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed  = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
  override def nextDouble : (Double, RNG) = {
    val (a, nextRNG) = nextInt
    (a.toDouble / Int.MaxValue, nextRNG)
  }
}

object RNG {
  def intDouble(rng : RNG) : ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = rng2.nextDouble
    ((i, d), rng3)
  }

  def ints(count : Int)(rng : RNG) : (List[Int], RNG) = {
    (0 until count).foldLeft((List.empty[Int], rng)) ((a, b) => {
      val (i, nrng) = rng.nextInt
      (a._1 :+ i, nrng)
    })
  }
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)
    println(ints(4)(rng))
//    val x : Rand[Int] = _.nextInt
//    println(x(rng))

  }
}