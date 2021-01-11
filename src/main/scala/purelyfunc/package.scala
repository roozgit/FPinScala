import purelyfunc.random.{RNG, State}

package object purelyfunc {
  type Rand[A] = State[RNG, A]
}
