import purelyfunc.random.RNG

package object purelyfunc {
  type Rand[+A] = RNG => (A, RNG)
}
