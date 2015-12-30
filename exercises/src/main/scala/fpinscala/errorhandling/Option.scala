package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(b) => Some(f(b))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(_) => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  def isDefined(): Boolean = this match {
    case None => false
    case _ => true
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    val m: Option[Double] = mean(xs)
    val md: Double = m.getOrElse(0)
    xs match {
      case Seq() => None
      case s => Some(xs.map((x) => math.pow(x - md, 2)).sum / xs.size)
    }

  }

  def variance2(xs: Seq[Double]): Option[Double] =
      mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa =>
    b map (bb =>
      f(aa, bb)))

  def map2ViaFor[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)


  def sequenceViaFoldRight[A](a: List[Option[A]]): Option[List[A]] = {
    val b: List[A] = a.foldRight(List[A]())((b, acc) => b match {
      case None => acc
      case Some(v) => v :: acc
    })
    b match {
      case Nil => None
      case _ if (a.length != b.length) => None
      case _ => Some(b)
    }
  }

  def traverseViaFoldRight[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {

    val b: List[B] = a.foldRight(List[B]())((b, acc) => f(b) match {
      case None => acc
      case Some(v) => v :: acc
    })

    b match {
      case Nil => None
      case _ if (a.length != b.length) => None
      case _ => Some(b)
    }
  }
}
