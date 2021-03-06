package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (a, rng2) = rng.nextInt
    (Math.abs(a), rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (a, rng2) = nonNegativeInt(rng)
    (a.toDouble/(Int.MaxValue.toDouble + 1), rng2)

  }

  def doubleWithMap(rng: RNG): Rand[Double] = map(nonNegativeInt) (a => a.toDouble/(Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (aint, rng2) = rng.nextInt
    val (adouble, rng3) = double(rng2)
    ((aint, adouble), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (adouble, rng2) = double(rng)
    val (aint, rng3) = rng.nextInt
    ((adouble, aint), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (adouble1, rng1) = double(rng)
    val (adouble2, rng2) = double(rng1)
    val (adouble3, rng3) = double(rng2)
    ((adouble1, adouble2, adouble3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count:Int, rng: RNG, list: List[Int]): (List[Int], RNG) = {
      if (count == 0) (list, rng)
      else {
        val (i, rng1) = rng.nextInt
        go(count - 1, rng1, i :: list)
      }
    }
    go(count, rng, List[Int]())
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rang) = ra(rng)
      val (b, rbng) = rb(rang)
      (f(a,b), rbng)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))



  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r1) = f(rng)
      val f2 = g(a)
      val (a2, r2) = f2(r1)
      (a2, r2)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))

}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State{
    s => {
      val(a, s1) = run(s)
      (f(a), s1)
    }
  }


  def mapWithFlatMap[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))


  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State {
      s => {
        val(a, s1) = run(s)
        val(b, s2) = sb.run(s1)
        (f(a,b), s2)
      }
    }

  def map2WithFlatMap[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State {
      s => {
        val(a, s1) = run(s)
        f(a).run(s1)
      }
    }

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

}

object State {
  type Rand[A] = State[RNG, A]

  def sequenceViaFoldRight[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) => (s: Machine) =>
    (i,s) match {
      case (_, Machine(_, 0, _)) => s
    }

}

