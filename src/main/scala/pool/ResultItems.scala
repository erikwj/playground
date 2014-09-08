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

    def rootValue: A //Monoid? needs append methond
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

    //    def empty: A
    //
    //    def u[B>:A](q: A): B
    //
    //    def drop[B](qs: A, q: B):B
    //
    //    def contains[B>:A](qs: A,q:B): Boolean

    //    /**
    //     * Update: updates a Pool with an answers and returns a new Pool
    //     */
    //    def update[B>:A](b: Boolean)(q: B)(f: (B, Result[A]) => Pool[B]): Pool[B] = {
    //
    //      def updateA[B](q: B, t: Stream[Pool[A]])(f: (B, Result[A]) => Pool[B]): Stream[Pool[B]] = t match {
    //        case Stream.Empty => Stream(f(q, Result.Empty.apply))
    //        case _ => Stream(f(q, t.head.result))
    //      }
    //
    //      def updateCorrect(p: Stream[Pool[A]])(b: Boolean)(q: B)(f: (B, Result[A]) => Pool[B]): Stream[Pool[B]] =
    //        if (b) updateA(q, p)(f) else Stream(f(p.head.qs, p.head.result))
    //
    //      def updateInCorrect(p: Stream[Pool[A]])(b: Boolean)(q: B)(f: (B, Result[A]) => Pool[B]): Stream[Pool[B]] =
    //        if (!b) updateA(q, p)(f) else Stream(f(p.head.qs, p.head.result))
    //
    //      this match {
    //        case tc: Correct[A] => correct(drop(qs,q), Result(updateInCorrect(tc.result.left)(b)(q)(f), updateCorrect(tc.result.right)(b)(q)(f)))
    //        case ti: InCorrect[A] => incorrect(drop(qs,q), Result(updateInCorrect(ti.result.left)(b)(q)(f), updateCorrect(ti.result.right)(b)(q)(f)))
    //      }
    //    }

    //    def updater(b:Boolean) = update(b) _
    //    
    //    def updateBranch(a:Answer[A]) = {
    //      val u = updater(a.r)
    //      this flatMap {p => u(a.q)}
    //    }
    //    

    //    private val z: Stream[A] = Stream.Empty
    //
    //    private def someQs(p: Stream[Pool[A]]): A = p match {
    //      case Stream.Empty => Stream.Empty
    //      case _ => p.head.qs
    //    }
    //
    //    def levels: Stream[A] = fold(someQs)(_.append(_))

    //    def next: Stream[A] = folder(Stream(this))(z)((p: Pool[A], s: Stream[A]) => s.append(p.qs))(x => !x.isEmpty)

        def next = levels.flatten

    //    def find(q: A): Pool[A] = finder(Stream(this))((p: Pool[A]) => p)(_.contains(q))

    //    def diff(p1: Pool[A], p2: Pool[A]) = ???
    //    def corrects: Int = ???
    //    def incorrects: Int = ???

    /**
     *
     * Map
     *
     */
    def map[B](f: A => B): Pool[B] = {

      /**
       * try Stream[InCorrect] => only lefts, Stream[Correct] => only rights, Stream[Pool] => Mixed
       */

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
     *  def flatMap[B](f: A => Tree[B]): Tree[B] = {
     *  val r: Tree[B] = f(rootLabel)
     *  Tree.node(r.rootLabel, r.subForest #::: subForest.map(_.flatMap(f)))
     *  }
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

    def unfoldForest[A, B](s: Stream[A])(f: A => (B, () => Stream[A])): Stream[Pool[B]] =
      s.map(unfoldTree(_)(f))

    def unfoldTree[A, B](v: A)(f: A => (B, () => Stream[A])): Pool[B] =
      this match {
        case i: InCorrect[A] => f(v) match {
          case (a, bs) => incorrect(a, unfoldForest(bs.apply())(f))
        }
        case c: Correct[A] => f(v) match {
          case (a, bs) => correct(a, unfoldForest(bs.apply())(f))
        }
      }

    //    def mergeResult[A](l: Stream[Pool[A]], r: Stream[Pool[A]])(f: (A, A) => A): Stream[Pool[A]] = {
    //      l match {
    //        case Stream.Empty => r
    //        case _ => l.head match {
    //          case x: InCorrect[A] => r match {
    //            case Stream.Empty => l
    //            case _ => r.head match {
    //              case i: InCorrect[A] => Stream(Pool.incorrect(f(l.head.rootValue, i.rootValue), mergeResult(l.head.result, i.result)(f)))
    //              case c: Correct[A] => Stream(l.head, c)
    //            }
    //          }
    //          case x: Correct[A] => mergeResult(r, Stream(x))(f)
    //        }
    //      }
    //    }
    //    /**
    //     * Fold
    //     */
    //
    //    def fold[B](f: Stream[Pool[A]] => B)(g: (B, B) => B): B = {
    //      def foldL[B](t: Option[InCorrect[A,B]])(f: Stream[Pool[A]] => B)(g: (B, B) => B): B = t match {
    //        case Some(p) => folds[B, InCorrect[A]](p)(f)(g)
    //        case _ => f(Stream.Empty)
    //      }
    //
    //      def foldR[B](t: Option[Correct[A]])(f: Stream[Pool[A]] => B)(g: (B, B) => B): B = t match {
    //        case Some(p) => folds[B, Correct[A]](p)(f)(g)
    //        case _ => f(Stream.Empty)
    //      }
    //
    //      def opsI(p: Option[Pool[A]]): Option[InCorrect[A]] = p map {
    //        case i: InCorrect[A] => i
    //      }
    //
    //      def opsC(p: Option[Pool[A]]): Option[Correct[A]] = p map {
    //        case c: Correct[A] => c
    //      }
    //
    //      def folds[B, T <: Pool[A]](t: T)(f: Stream[Pool[A]] => B)(g: (B, B) => B): B =
    //        t.result.flatten.toList match {
    //          case Nil => f(Stream(t))
    //          case List(x: InCorrect[A], y: Correct[A]) =>
    //            val r = t.result
    //            g(foldL(opsI(r.left.headOption))(f)(g), foldR(opsC(r.right.headOption))(f)(g))
    //          case List(c: Correct[A]) =>
    //            g(f(Stream(this)), foldR(Some(c))(f)(g))
    //          case List(i: InCorrect[A]) =>
    //            g(f(Stream(this)), foldL(Some(i))(f)(g))
    //        }
    //
    //      this match {
    //        case i: InCorrect[A] => foldL(Some(i))(f)(g)
    //        case c: Correct[A] => foldR(Some(c))(f)(g)
    //      }
    //    }
    //
    //    def depth = this.fold(_ => 1)((d1, d2) => 1 + (d1 max d2))

    //    def leafs: Stream[(A, String)] = {
    //      type B = Stream[(A, String)]
    //
    //      def counter(p: Stream[Pool[A]]): B = {
    //        p match {
    //          case Stream.Empty => Stream((empty, ""))
    //          case _ => Stream((p.head.qs, p.head.show))
    //        }
    //      }
    //      this.fold(counter(_))(_.append(_))
    //    }

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

  trait TreeFunctions {
    /** Construct a new Tree node. */
    def inode[A](root: => A, result: => Stream[Pool[A]]): Pool[A] = InCorrect(root, result)
    def cnode[A](root: => A, result: => Stream[Pool[A]]): Pool[A] = Correct(root, result)

    //    def node[A](root: => A, score: => Stream[Pool[A]]): Pool[A] = new Pool[A] {
    //      lazy val rootValue = root
    //      lazy val result = score
    //      override def toString = "<pool>"
    //    }

    /** Construct a tree node with no children. */
    def ileaf[A](root: => A): Pool[A] = inode(root, Stream.empty)
    def cleaf[A](root: => A): Pool[A] = cnode(root, Stream.empty)

  }

}