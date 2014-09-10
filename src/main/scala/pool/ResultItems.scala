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

    
        /**
     * Update: updates a Pool with an answers and returns a new Pool
     */
//        def update[B](b:Boolean)(a: A)(d:(Stream[A],B) => Stream[B]): Pool[B] = {
    
//          def updateA[T <: Pool[A]](q: Stream[A], t: Stream[T])(f: (Stream[A], Result[A]) => Stream[T]) = t match {
//            case Stream.Empty => f(q, Result.Empty.apply)
//            case _ => f(t.head.appendQ(q), t.head.result)
//          }
//    
//          def updateCorrect(p: Stream[Pool[A]])(q:Stream[A])(b:Boolean): Stream[Pool[A]] =
//            if (b) updateA(q, p)((q, r) => Stream(correct(q, r))) else p
//            
//          def updateInCorrect(p: Stream[Pool[A]])(q:Stream[A])(b:Boolean): Stream[Pool[A]] =
//            if (!b) updateA(q, p)((q,r) => Stream(incorrect(q, r))) else p
//    
//          if (contains(a.head)) this match {
//            case tc: Correct[A] => correct(dropQ(a.head), Result(updateInCorrect(tc.result.left)(a)(b), updateCorrect(tc.result.right)(a)(b)))
//            case ti: InCorrect[A] => incorrect(dropQ(a.head), Result(updateInCorrect(ti.result.left)(a)(b), updateCorrect(ti.result.right)(a)(b)))
//          }
//          else this
    
//        }
    
    
    //    def merge[B >: A](b:B):B

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

    /** Construct a new Tree node. */
    def inode[A](root: => A, result: => Stream[Pool[A]]): Pool[A] = InCorrect(root, result)
    def cnode[A](root: => A, result: => Stream[Pool[A]]): Pool[A] = Correct(root, result)

    /** Construct a tree node with no children. */
    def ileaf[A](root: => A): Pool[A] = inode(root, Stream.empty)
    def cleaf[A](root: => A): Pool[A] = cnode(root, Stream.empty)

    def apply[A](qs: => A): Pool[A] = incorrect(qs, Stream.Empty)

    def mergeResult[A](l: Stream[Pool[A]], r: => Stream[Pool[A]])(implicit f: (A, A) => A): Stream[Pool[A]] = {
      l match {
        case Stream.Empty => r
        case _ => l.head match {
          case i: InCorrect[A] => r match {
            case Stream.Empty => l
            case _ => r.head match {
              case i: InCorrect[A] => Stream(incorrect(f(l.head.rootValue, i.rootValue), mergeResult(l.head.result, i.result)(f))).append(mergeResult(l.tail, r.tail)(f))
              case c: Correct[A] => (mergeResult(Stream(l.head), r.tail)(f)).append(mergeResult(l.tail, Stream(c))(f))
            }
          }
          case c: Correct[A] => r match {
            case Stream.Empty => l
            case _ => r.head match {
              case i: InCorrect[A] => (mergeResult(l.tail, Stream(i))(f)).append(mergeResult(Stream(l.head), r.tail)(f))
              case c: Correct[A] => (mergeResult(l.tail, r.tail)(f)).append(Stream(correct(f(l.head.rootValue, c.rootValue), mergeResult(l.head.result, c.result)(f))))
            }
          }
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