package pool

import scalaz._
import Scalaz._
import std.stream.{ streamInstance, streamMonoid }
import std.string.stringInstance

object ResultItems {

  case class Answer[A](q: A, r: Boolean) {
  }

  sealed abstract class Pool[+A] {
    import Pool._

    //    def current: A //Monoid? needs append method
    //    def box: Stream[A]
    def result: Stream[Pool[A]]

    def rootValues: Stream[A] = this match {
      case InCorrect(q, qs, _) => Stream(q).append(qs)
      case Correct(q, qs, _) => Stream(q).append(qs)
      case _ => Stream.Empty
    }

    /**
     * Update: updates a Pool with an answers and returns a new Pool
     */
    //    def update[B](b: Boolean)(a: A): Pool[B] = {
    //      this match {
    //        //Branches or leafs
    //        case i:InCorrect[A] => i match {
    //          case InCorrect(q, Stream.Empty, r) => if(a == q) i.clone(result = r) else InCorrect(q, Stream.Empty, r map {_ update(b)(a)})
    //        }
    //        case InCorrect(q, qs, r) =>
    //        case Correct(q, Stream.Empty, r) => 
    //        case Correct(q, qs, r) =>
    //        case _
    //      }

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

    //    }

    //    def merge[B >: A](b:B):B

    //    /** Maps the elements of the Tree into a Monoid and folds the resulting Tree. */
    //    def foldMap[B: Monoid](f: A => B): B =
    //      Monoid[B].append(f(rootValue), Foldable[Stream].foldMap[Pool[A], B](result)((_: Pool[A]).foldMap(f)))
    //
    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      Foldable[Stream].foldRight(flatten, z)(f)

    /** Pre-order traversal. */
    def flatten: Stream[A] = {
      //      val estream: Stream[A] = Stream.Empty
      def squish(pool: Pool[A], xs: Stream[A]): Stream[A] = pool match {
        //Branches or leafs
        case InCorrect(q, Stream.Empty, r) => Stream.cons(q, Foldable[Stream].foldr[Pool[A], Stream[A]](pool.result, xs)(a => b => squish(a, b)))
        case InCorrect(q, qs, r) => {
          val qqs = Stream(q).append(qs)
          qqs.init.append(Stream.cons(qqs.last, Foldable[Stream].foldr[Pool[A], Stream[A]](pool.result, xs)(a => b => squish(a, b))))
        }

        case Correct(q, Stream.Empty, r) => Stream.cons(q, Foldable[Stream].foldr[Pool[A], Stream[A]](pool.result, xs)(a => b => squish(a, b)))
        case Correct(q, qs, r) => {
          val qqs = Stream(q).append(qs)
          qqs.init.append(Stream.cons(qqs.last, Foldable[Stream].foldr[Pool[A], Stream[A]](pool.result, xs)(a => b => squish(a, b))))
        }

        //Trunk
        case _ => (result map { p => p flatten }).flatten
      }
      squish(this, Stream.Empty)
    }

    /** Breadth-first traversal. */
    def levels = {
      val f = (s: Stream[Pool[A]]) => {
        Foldable[Stream].foldMap(s)((_: Pool[A]).result)
      }
      Stream.iterate(Stream(this))(f) takeWhile (!_.isEmpty) map (_ map (_.rootValues))
    }

    //
    def next = levels.flatten

    /**
     *
     * Map
     *
     */
    def map[B](f: A => B): Pool[B] = {

      this match {
        case ITrunk(r) => ITrunk(r map { _ map f })
        case CTrunk(r) => CTrunk(r map { _ map f })
        case InCorrect(q, qs, r) => (q, qs, r) match {
          case (q, Stream.Empty, Stream.Empty) => Pool.incorrect(f(q), Stream.Empty, Stream.Empty)
          case (q, qs, Stream.Empty) => Pool.incorrect(f(q), qs map f, Stream.Empty)
          case (q, qs, r) => Pool.incorrect(f(q), qs map f, r map { _ map f })
        }
        case Correct(q, qs, r) => (q, qs, r) match {
          case (q, Stream.Empty, Stream.Empty) => Pool.correct(f(q), Stream.Empty, Stream.Empty)
          case (q, qs, Stream.Empty) => Pool.correct(f(q), qs map f, Stream.Empty)
          case (q, qs, r) => Pool.correct(f(q), qs map f, r map { _ map f })
        }
      }
    }

    /**
     *
     * QMap
     *
     */
    def qmap[B](f: Stream[A] => Stream[B]): Pool[B] = {

      this match {
        case ITrunk(r) => ITrunk(r map { _ qmap f })
        case CTrunk(r) => CTrunk(r map { _ qmap f })
        case InCorrect(q, qs, r) => (q, qs, r) match {
          case (q, Stream.Empty, r) =>
            val res = f(Stream(q))
            res match {
              case Stream.Empty => ITrunk(r map { _ qmap f })
              case _ => inode(res.head, res.tail, r map { _ qmap f })
            }
          case (q, qs, r) =>
            val res = f(Stream(q) #::: qs)
            Pool.incorrect(res.head, res.tail, r map { _ qmap f })
        }
        case Correct(q, qs, r) => (q, qs, r) match {
          case (q, Stream.Empty, r) =>
            val res = f(Stream(q))
            res match {
              case Stream.Empty => CTrunk(r map { _ qmap f })
              case _ => cnode(res.head, res.tail, r map { _ qmap f })
            }
          case (q, qs, r) =>
            val res = f(Stream(q) #::: qs)
            cnode(res.head, res.tail, r map { _ qmap f })
        }

      }
    }

    //    /**
    //     *
    //     * flatMap
    //     *
    //     */
    //    def flatMap[B](f: A => Pool[B]): Pool[B] = {
    //      this match {
    //        case ITrunk(r) => ITrunk(r map { _ flatMap f })
    //        case CTrunk(r) => CTrunk(r map { _ flatMap f })
    //        case InCorrect(q, qs, r) => (q, qs, r) match {
    //          case (q, Stream.Empty, Stream.Empty) => Pool.incorrect(f(q), Stream.Empty, Stream.Empty)
    //          case (q, qs, Stream.Empty) => Pool.incorrect(f(q), qs map f, Stream.Empty)
    //          case (q, qs, r) => Pool.incorrect(f(q), qs map f, r map { _ map f })
    //        }
    //        case Correct(q, qs, r) => (q, qs, r) match {
    //          case (q, Stream.Empty, Stream.Empty) => Pool.correct(f(q), Stream.Empty, Stream.Empty)
    //          case (q, qs, Stream.Empty) => Pool.correct(f(q), qs map f, Stream.Empty)
    //          case (q, qs, r) => Pool.correct(f(q), qs map f, r map { _ map f })
    //        }
    //
    //        case ti: InCorrect[A] =>
    //          val r = f(ti.current)
    //          Pool.incorrect(r.current, r.result #::: ti.result.map(_.flatMap(f)))
    //        case tc: Correct[A] =>
    //          val r = f(tc.current)
    //          Pool.correct(r.current, r.result #::: tc.result.map(_.flatMap(f)))
    //      }
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
      case x: ITrunk[A] => "I"
      case x: CTrunk[A] => "I"
    }
  }

  case class InCorrect[A](current: A, box: Stream[A], result: Stream[Pool[A]]) extends Pool[A]
  case class Correct[A](current: A, box: Stream[A], result: Stream[Pool[A]]) extends Pool[A]
  case class ITrunk[A](result: Stream[Pool[A]]) extends Pool[A]
  case class CTrunk[A](result: Stream[Pool[A]]) extends Pool[A]

  object Pool {

    def incorrect[A, B](q: A, qs: Stream[A], r: Stream[Pool[A]]): Pool[A] = InCorrect(q, qs, r)
    def correct[A, B](q: A, qs: Stream[A], r: Stream[Pool[A]]): Pool[A] = Correct(q, qs, r)
    //
    /** Construct a new Trunk with no values. */
    def itrunk[A, B](result: => Stream[Pool[A]]): Pool[A] = ITrunk(result)
    def ctrunk[A](result: => Stream[Pool[A]]): Pool[A] = CTrunk(result)
    //
    /** Construct a new Tree node. */
    def inode[A](root: => A, box: => Stream[A], result: => Stream[Pool[A]]): Pool[A] = InCorrect(root, box, result)
    def cnode[A](root: => A, box: => Stream[A], result: => Stream[Pool[A]]): Pool[A] = Correct(root, box, result)

    /** Construct a tree node with no children. */
    def ileaf[A](root: => A): Pool[A] = inode(root, Stream.empty, Stream.empty)
    def cleaf[A](root: => A): Pool[A] = cnode(root, Stream.empty, Stream.empty)
    //
    //        def apply[A](q: => A, qs: => Stream[A]): Pool[A] = incorrect(q,qs, Stream.Empty)

    //    def mergeResult[A](l: Stream[Pool[A]], r: => Stream[Pool[A]])(implicit f: (A, A) => A): Stream[Pool[A]] = {
    //      l match {
    //        case Stream.Empty => r
    //        case _ => l.head match {
    //          case i: InCorrect[A] => r match {
    //            case Stream.Empty => l
    //            case _ => r.head match {
    //              case i: InCorrect[A] => Stream(incorrect(f(l.head.current, i.current), mergeResult(l.head.result, i.result)(f))).append(mergeResult(l.tail, r.tail)(f))
    //              case c: Correct[A] => (mergeResult(Stream(l.head), r.tail)(f)).append(mergeResult(l.tail, Stream(c))(f))
    //            }
    //          }
    //          case c: Correct[A] => r match {
    //            case Stream.Empty => l
    //            case _ => r.head match {
    //              case i: InCorrect[A] => (mergeResult(l.tail, Stream(i))(f)).append(mergeResult(Stream(l.head), r.tail)(f))
    //              case c: Correct[A] => (mergeResult(l.tail, r.tail)(f)).append(Stream(correct(f(l.head.current, c.current), mergeResult(l.head.result, c.result)(f))))
    //            }
    //          }
    //        }
    //      }
    //    }

    def updateResult[A, B >: A](b1: Boolean)(a: B)(r: Stream[Pool[B]]): Stream[Pool[B]] = {
      def appendInCorrect[B >: A](p: Pool[B], a: B): Pool[B] =
        p match {
          case i: InCorrect[A] => InCorrect(i.current, i.box #::: Stream(a), i.result)
          case _ => ileaf(a)
        }

      def appendCorrect[B >: A](p: Pool[B], a: B): Pool[B] =
        p match {
          case c: Correct[A] => Correct(c.current, c.box #::: Stream(a), c.result)
          case _ => cleaf(a)
        }

      r match {
        case Stream(ii, cc) => if (!b1) Stream(appendInCorrect(ii, a), cc) else Stream(ii, appendCorrect(cc, a))
        case Stream(p) => p match {
          case i: InCorrect[A] => if (!b1) Stream(appendInCorrect(i, a)) else Stream(i, appendCorrect(p, a))
          case c: Correct[A] => if (b1) Stream(appendCorrect(c, a)) else Stream(appendInCorrect(p, a), c)
          case _ => if (b1) Stream(cleaf(a)) else Stream(ileaf(a))
        }
        case _ => if (b1) Stream(cleaf(a)) else Stream(ileaf(a))
      }
    }

    def drop[A, B >: A](a: B)(s: Stream[A]): Stream[B] = s match {
      case h #:: t if (h == a) => t
      case _ => println("A = " + a)
      s
    }
    
    def drops[A,B>:A](a: B) = drop[A,B](a) _ 

    //          p match {
    //      case InCorrect(q, Stream.Empty, r) if(a == q) => ITrunk(r)
    //      case InCorrect(q, qs, r) if(a == q) => inode(qs.head, qs.tail, r)
    //      case Correct(q, Stream.Empty, r) if(a == q) => CTrunk(r)
    //      case Correct(q, qs, r) if(a == q) => cnode(qs.head, qs.tail, r)
    //      case _ => p
    //    }

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

}