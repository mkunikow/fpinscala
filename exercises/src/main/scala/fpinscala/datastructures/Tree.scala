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
    case Branch(a,b) => maximum(a) max maximum(b)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(a, b) => 1 + (depth(a) max depth(b))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(a,b) => Branch(map(a)(f), map(b)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(a,b) => g(fold(a)(f)(g), fold(b)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_=>1)((x,y)=>x+y+1)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_=>1)((x,y) => 1 + (x max y))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(x=>Leaf(f(x)):Tree[B])((x,y) => Branch(x, y))
}