package pool

object LTreez {

/**
 * Created by erikjanssen on 01/08/14.
 */

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](v:Leaf[A], left: Tree[A], right: Tree[A]) extends Tree[A]


  object Tree {

    /*
    Counts number of nodes (leaves and branches) in a tree
     */
    def size[A](t:Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(v,l,r) => 1 + size(l) + size(r)
    }

    def maximum(t: Tree[Int]):Int = t match {
      case Leaf(v) => v
      case Branch(v,l,r) => maximum(l) max maximum(r)
    }

    def depth[A](t: Tree[A]):Int = t match {
      case Leaf(_) => 1
      case Branch(v,l,r) => 1 + (depth(l) max depth(r))
    }

    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(lf,l,r) => Branch(Leaf(f(lf.value)), map(l)(f),map(r)(f))
    }

    def fold[A,B](t: Tree[A])(f: A => B)(g:(B,B) => B): B = t match {
      case Leaf(v) => f(v)
      case Branch(v,l,r) => g(fold(l)(f)(g),fold(r)(f)(g))
    }

    def sizeViaFold[A](t:Tree[A]): Int =
      fold(t)((x) => 1)(1 + _ + _)

    def maximumViaFold(t: Tree[Int]): Int =
      fold(t)(a => a)(_ max _)

    def depthViaFold[A](t: Tree[A]):Int =
      fold(t)(x => 0)((d1,d2) => 1 + (d1 max d2))
  }
}

  
  
  
