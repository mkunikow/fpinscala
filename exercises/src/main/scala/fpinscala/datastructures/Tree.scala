package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(a, b) => 1 + size(a) + size(b)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(a,b) => math.max(maximum(a), maximum(b))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(a, b) => 1 + math.max(depth(a), depth(b))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(a,b) => Branch(map(a)(f), map(b)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = ???

  def sizeViaFold[A](t: Tree[A]): Int = ???

  def maximumViaFold(t: Tree[Int]): Int = ???

  def depthViaFold[A](t: Tree[A]): Int = ???

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = ???
}