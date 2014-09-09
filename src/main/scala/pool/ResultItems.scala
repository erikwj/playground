package pool

import scalaz._
import Scalaz._
import std.stream.{ streamInstance, streamMonoid }
import std.string.stringInstance

object ResultItems {

  case class Answer[A](q: Stream[A], r: Boolean) {
    require(!q.isEmpty, q.length == 1)
  }

  sealed abstract class Pool[+A] {
    import Pool._

    def rootValue: A //Monoid? needs append method
    def result: Stream[Pool[A]]

    /** Maps the elements of the Tree into a Monoid and folds the resulting Tree. */
    def foldMap[B: Monoid](f: A => B): B =
      Monoid[B].append(f(rootValue), Foldable[Stream].foldMap[Pool[A], B](result)((_: Pool[A]).foldMap(f)))

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      Foldable[Stream].foldRight(flatten, z)(f)

    /** Pre-order traversal. */
    def flatten = {
      def squish(pool: Pool[A], xs: Stream[A]): Stream[A] =
        Stream.cons(pool.rootValue, Foldable[Stream].foldr[Pool[A], Stream[A]](pool.result, xs)(a => b => squish(a, b)))
      squish(this, Stream.Empty)
    }

    /** Breadth-first traversal. */
    def levels = {
      val f = (s: Stream[Pool[A]]) => {
        Foldable[Stream].foldMap(s)((_: Pool[A]).result)
      }
      Stream.iterate(Stream(this))(f) takeWhile (!_.isEmpty) map (_ map (_.rootValue))
    }

    def next = levels.flatten

    /**
     *
     * Map
     *
     */
    def map[B](f: A => B): Pool[B] = {

      this match {
        case InCorrect(q, r) => (q, r) match {
          case (q, Stream.Empty) => Pool.incorrect(f(q), Stream.Empty)
          case (q, _) => Pool.incorrect(f(q), r map (_ map f))
        }
        case Correct(q, r) => (q, r) match {
          case (q, Stream.Empty) => Pool.correct(f(q), Stream.Empty)
          case (q, _) => Pool.correct(f(q), r map (_ map f))
        }
      }
    }

    /**
     *
     * flatMap
     *
     */
    def flatMap[B](f: A => Pool[B]): Pool[B] = {
      this match {
        case ti: InCorrect[A] =>
          val r = f(rootValue)
          Pool.incorrect(r.rootValue, r.result #::: ti.result.map(_.flatMap(f)))
        case tc: Correct[A] =>
          val r = f(rootValue)
          Pool.correct(r.rootValue, r.result #::: tc.result.map(_.flatMap(f)))
      }
    }

    //    def unfoldForest[A, B](s: Stream[A])(f: A => (B, () => Stream[A])): Stream[Pool[B]] =
    //      s.map(unfoldTree(_)(f))
    //
    //    def unfoldTree[A, B](v: A)(f: A => (B, () => Stream[A])): Pool[B] =
    //      this match {
    //        case i: InCorrect[A] => f(v) match {
    //          case (a, bs) => incorrect(a, unfoldForest(bs.apply())(f))
    //        }
    //        case c: Correct[A] => f(v) match {
    //          case (a, bs) => correct(a, unfoldForest(bs.apply())(f))
    //        }
    //      }
    /**
     *
     *
     *
     *
     */
    def show = this match {
      case x: InCorrect[A] => "<I>"
      case x: Correct[A] => "<C>"
    }

  }

  case class InCorrect[A](rootValue: A, result: Stream[Pool[A]]) extends Pool[A]
  case class Correct[A](rootValue: A, result: Stream[Pool[A]]) extends Pool[A]

  object Pool {

    def incorrect[A](qs: A, r: Stream[Pool[A]]): Pool[A] = InCorrect(qs, r)
    def correct[A](qs: A, r: Stream[Pool[A]]): Pool[A] = Correct(qs, r)

    def apply[A](qs: => A): Pool[A] = incorrect(qs, Stream.Empty)

    def mergeResult[A](l: Stream[Pool[A]], r: => Stream[Pool[A]])(f: (A, A) => A): Stream[Pool[A]] = {
      l match {
        case Stream.Empty => r
        case Stream(ii) => ii match {
          case i: InCorrect[A] => r match {
            case Stream.Empty => l
            case Stream(ii, cc) => ii match {
              //merge results
              case i: InCorrect[A] => Stream(Pool.incorrect(f(l.head.rootValue, i.rootValue), mergeResult(l.head.result, i.result)(f)), cc)
              //append 
              case c: Correct[A] => Stream(l.head, c)
            }
          }
          case c: Correct[A] => mergeResult(r, Stream(c))(f)
        }
        case Stream(ii, cc) => ii match {
          case i: InCorrect[A] => r match {
            case Stream.Empty => l
            case _ => r.head match {
              //merge results
              case i: InCorrect[A] => Stream(Pool.incorrect(f(l.head.rootValue, i.rootValue), mergeResult(l.head.result, i.result)(f)), cc)
              //append 
              case c: Correct[A] => Stream(l.head, Pool.correct(f(cc.rootValue, c.rootValue), mergeResult(cc.result, c.result)(f)))
            }
          }
          case c: Correct[A] => mergeResult(r, Stream(c))(f)
        }
      }
    }
  }

  //  sealed abstract class PoolInstances {
  //    implicit val poolInstance: Traverse1[Pool] with Monad[Pool] with Comonad[Pool] = new Traverse1[Pool] with Monad[Pool] with Comonad[Pool] {
  //      def point[A](a: => A): Tree[A] = Tree.leaf(a)
  //      def cobind[A, B](fa: Tree[A])(f: Tree[A] => B): Tree[B] = fa cobind f
  //      def copoint[A](p: Tree[A]): A = p.rootLabel
  //      override def map[A, B](fa: Tree[A])(f: A => B) = fa map f
  //      def bind[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa flatMap f
  //      def traverse1Impl[G[_]: Apply, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] = fa traverse1 f
  //      override def foldRight[A, B](fa: Tree[A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
  //      override def foldMapRight1[A, B](fa: Tree[A])(z: A => B)(f: (A, => B) => B) = (fa.flatten.reverse: @unchecked) match {
  //        case h #:: t => t.foldLeft(z(h))((b, a) => f(a, b))
  //      }
  //      override def foldLeft[A, B](fa: Tree[A], z: B)(f: (B, A) => B): B =
  //        fa.flatten.foldLeft(z)(f)
  //      override def foldMapLeft1[A, B](fa: Tree[A])(z: A => B)(f: (B, A) => B): B = fa.flatten match {
  //        case h #:: t => t.foldLeft(z(h))(f)
  //      }
  //      override def foldMap[A, B](fa: Pool[A])(f: A => B)(implicit F: Monoid[B]): B = fa foldMap f
  //    }
  //
  //    implicit def poolEqual[A](implicit A: Equal[A]): Equal[Pool[A]] = new Equal[Pool[A]] {
  //      def equal(a1: Pool[A], a2: Pool[A]): Boolean = {
  //        A.equal(a1.rootValue, a2.rootValue) && a1.result.corresponds(a2.result)(equal _)
  //      }
  //    }
  //
  //  }

  trait PoolFunctions {
    /** Construct a new Tree node. */
    def inode[A](root: => A, result: => Stream[Pool[A]]): Pool[A] = InCorrect(root, result)
    def cnode[A](root: => A, result: => Stream[Pool[A]]): Pool[A] = Correct(root, result)

    /** Construct a tree node with no children. */
    def ileaf[A](root: => A): Pool[A] = inode(root, Stream.empty)
    def cleaf[A](root: => A): Pool[A] = cnode(root, Stream.empty)

  }

}