package purelyfunc

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