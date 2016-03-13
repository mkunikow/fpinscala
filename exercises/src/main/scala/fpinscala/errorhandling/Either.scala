package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case lf@ Left(l) => lf
   case Right(r) => Right(f(r))

 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case lf@ Left(l) => lf
   case Right(r) => f(r)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(_) => b
   case Right(_) => this

 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
   for {
     aa <-this
     bb <- b
   } yield f(aa, bb)
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
//      case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
    }


  def traverse_1[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]] (Right(Nil)) ((a, acc) => f(a).map2(acc)(_ :: _))


  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  def sequence_1 [E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    es.foldRight[Either[E,List[A]]] (Right(Nil)) ((a, acc) => a.map2(acc)(_ :: _))

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}