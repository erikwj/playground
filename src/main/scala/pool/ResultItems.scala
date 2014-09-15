package pool

import scalaz._
import Scalaz._
import std.stream.{ streamInstance, streamMonoid }
import std.string.stringInstance

import scala.util.Either

object ResultItems {

  case class Answer[A](q: A, r: Boolean)

  sealed abstract class Pool[+A] {
    import Pool._

    def result: Stream[Pool[A]]

    def rootValues: Stream[A] = this match {
      case InCorrect(q, qs, _) => Stream.cons(q, qs)
      case Correct(q, qs, _) => Stream.cons(q, qs)
      case _ => Stream.Empty
    }

    /**
     * Update: updates a Pool with an answers and returns a new Pool
     */
    def update[B >: A](a: Answer[B]) = emap(dropA(a.q))(updateR(a))

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      Foldable[Stream].foldRight(flatten, z)(f)

    /** Pre-order traversal. */
    def flatten: Stream[A] = {
      def squish(pool: Pool[A], xs: Stream[A]): Stream[A] = {
        def squeez(s: Stream[A], r: Stream[Pool[A]]) = s.init.append(Stream.cons(s.last, Foldable[Stream].foldr[Pool[A], Stream[A]](pool.result, xs)(a => b => squish(a, b))))

        pool match {
          //Branches or leafs should be easier with case class Branch
          case InCorrect(q, qs, r) => squeez(Stream.cons(q, qs), r)
          case Correct(q, qs, r) => squeez(Stream.cons(q, qs), r)
          //Trunk
          case _ => (result map { _.flatten }).flatten
        }
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

      def mapper[A, B](q: A, qs: Stream[A], r: Stream[Pool[A]])(f: A => B)(g: (B, Stream[B], Stream[Pool[B]]) => Pool[B]) = {
        (q, qs, r) match {
          case (q, Stream.Empty, Stream.Empty) => g(f(q), Stream.Empty, Stream.Empty)
          case (q, qs, Stream.Empty) => g(f(q), qs map f, Stream.Empty)
          case (q, qs, r) => g(f(q), qs map f, r map { _ map f })
        }
      }

      this match {
        case ITrunk(r) => itrunk(r map { _ map f })
        case CTrunk(r) => ctrunk(r map { _ map f })
        case InCorrect(q, qs, r) => mapper(q, qs, r)(f)(inode(_, _, _))
        case Correct(q, qs, r) => mapper(q, qs, r)(f)(cnode(_, _, _))
      }
    }

    /**
     *
     * EMap : Maps over all elements
     * Function f returns Either[rootValues,rootValues]:
     * - Right: if element is effected and rootValues (q,qs) are updated => Function g is applied to result
     * - Left if element is not effected
     *
     */
    def emap[B](f: Stream[A] => scala.util.Either[Stream[B], Stream[B]])(g: Stream[Pool[B]] => Stream[Pool[B]]): Pool[B] = {
      this match {
        case ITrunk(r) => itrunk(r map { _.emap(f)(g) })
        case CTrunk(r) => ctrunk(r map { _.emap(f)(g) })
        case InCorrect(q, qs, r) =>
          f(Stream(q) #::: qs) match {
            case Right(updated) => updated match {
              case Stream.Empty => itrunk(g(r map { _.emap(f)(g) }))
              case _ => inode(updated.head, updated.tail, g(r map { _.emap(f)(g) }))
            }
            case Left(nochange) => inode(nochange.head, nochange.tail, r map { _.emap(f)(g) })
          }
        case Correct(q, qs, r) =>
          f(Stream(q) #::: qs) match {
            case Right(updated) => updated match {
              case Stream.Empty => ctrunk(g(r map { _.emap(f)(g) }))
              case _ => cnode(updated.head, updated.tail, g(r map { _.emap(f)(g) }))
            }
            case Left(nochange) => cnode(nochange.head, nochange.tail, r map { _.emap(f)(g) })
          }
      }
    }

    /**
     *
     * PoolMap : Maps over all elements
     * Function f returns Either[rootValues,rootValues]:
     * - Right: if element is effected and rootValues (q,qs) are updated => Function g is applied to result
     * - Left if element is not effected
     *
     */
    def pmap[B, S, D](s: S)(f: Pool[A] => (B, S))(g: (S, S) => S): Pool[(B, S)] = {

      def trace[B](p: Pool[A], st: S)(f: Pool[A] => (B, S))(fp: ((B, S), Stream[Pool[(B, S)]]) => Pool[(B, S)]) = {
        val r = f(p)
        val sit = g(s, r._2)
        fp((r._1, sit), p.result map { _.pmap(sit)(f)(g) })
      }

      this match {
        case i: ITrunk[A] => trace(i,s)(f)(inode(_,Stream.Empty,_))
        case i: InCorrect[A] => trace(i,s)(f)(inode(_,Stream.Empty,_))
        case c: CTrunk[A] => trace(c,s)(f)(cnode(_,Stream.Empty,_))
        case c: Correct[A] =>trace(c,s)(f)(cnode(_,Stream.Empty,_))
      }
    }

    def show = this match {
      case x: InCorrect[A] => "I"
      case x: Correct[A] => "C"
      case x: ITrunk[A] => "I"
      case x: CTrunk[A] => "I"
    }
  }

  //Can be a leaf with empty result or a branch with some result
  case class InCorrect[A](current: A, box: Stream[A], result: Stream[Pool[A]]) extends Pool[A]
  case class Correct[A](current: A, box: Stream[A], result: Stream[Pool[A]]) extends Pool[A]
  //Trunks have no leafs and thus no values
  case class ITrunk[A](result: Stream[Pool[A]]) extends Pool[A]
  case class CTrunk[A](result: Stream[Pool[A]]) extends Pool[A]

  object Pool {

    //
    /** Construct a new Trunk with no values. */
    def itrunk[A](result: => Stream[Pool[A]]): Pool[A] = ITrunk(result)
    def ctrunk[A](result: => Stream[Pool[A]]): Pool[A] = CTrunk(result)
    //
    /** Construct a new Tree node. */
    def inode[A](root: => A, box: => Stream[A], result: => Stream[Pool[A]]): Pool[A] = InCorrect(root, box, result)
    def cnode[A](root: => A, box: => Stream[A], result: => Stream[Pool[A]]): Pool[A] = Correct(root, box, result)

    /** Construct a tree node with one value and no children. */
    def ileaf[A](root: => A): Pool[A] = inode(root, Stream.empty, Stream.empty)
    def cleaf[A](root: => A): Pool[A] = cnode(root, Stream.empty, Stream.empty)
    //
    /** Construct a tree node with multiple values and no children. */
    def ileafs[A](root: => A, values: => Stream[A]): Pool[A] = inode(root, values, Stream.empty)
    def cleafs[A](root: => A, values: => Stream[A]): Pool[A] = cnode(root, values, Stream.empty)
    //
    //        def apply[A](q: => A, qs: => Stream[A]): Pool[A] = incorrect(q,qs, Stream.Empty)

    def merge[A](l: Pool[A], r: => Pool[A])(implicit f: (Stream[A], Stream[A]) => Stream[A]): Pool[A] = {
      (l, r) match {
        case (i: InCorrect[A], i2: InCorrect[A]) => inode(i.current, i.box.append(Stream.cons(i2.current, i2.box)), smerge(i.result, i2.result))
        case (i: InCorrect[A], c: Correct[A]) => itrunk(smerge(Stream(i), Stream(c)))
        case (c: Correct[A], c2: Correct[A]) => cnode(c.current, c.box.append(Stream.cons(c2.current, c2.box)), smerge(c.result, c2.result))
        case (c: Correct[A], i: InCorrect[A]) => itrunk(smerge(Stream(i), Stream(c)))
        case (it: ITrunk[A], i: InCorrect[A]) => inode(i.current, i.box, smerge(it.result, i.result))
        case (it: ITrunk[A], c: Correct[A]) => cnode(c.current, c.box, smerge(it.result, c.result))
        case (ct: CTrunk[A], i: InCorrect[A]) => inode(i.current, i.box, smerge(ct.result, i.result))
        case (ct: CTrunk[A], c: Correct[A]) => cnode(c.current, c.box, smerge(ct.result, c.result))
        case (i: InCorrect[A], it: ITrunk[A]) => inode(i.current, i.box, smerge(i.result, it.result))
        case (i: InCorrect[A], ct: CTrunk[A]) => inode(i.current, i.box, smerge(i.result, ct.result))
        case (c: Correct[A], it: ITrunk[A]) => cnode(c.current, c.box, smerge(c.result, it.result))
        case (c: Correct[A], ct: CTrunk[A]) => cnode(c.current, c.box, smerge(c.result, ct.result))
        case (it: ITrunk[A], it2: ITrunk[A]) => ITrunk(smerge(it.result, it2.result))
        case (ct: CTrunk[A], ct2: CTrunk[A]) => ITrunk(smerge(ct.result, ct2.result))
        case (it: ITrunk[A], ct: CTrunk[A]) => ITrunk(smerge(it.result, ct.result))
        case (ct: CTrunk[A], it: ITrunk[A]) => ITrunk(smerge(it.result, ct.result))
      }
    }

    def smerge[A](l: Stream[Pool[A]], r: => Stream[Pool[A]])(implicit f: (Stream[A], Stream[A]) => Stream[A]): Stream[Pool[A]] = {
      l match {
        case Stream.Empty => r
        case _ => l.head match {
          case InCorrect(ilq, ilqs, ilr) =>
            r match {
              case Stream.Empty => l
              case _ => r.head match {
                case InCorrect(irq, irqs, irr) =>
                  //rootValues
                  val rvs: Stream[A] = f(Stream.cons(ilq, ilqs), Stream.cons(irq, irqs))
                  rvs match {
                    case Stream.Empty => Stream(itrunk(smerge(ilr, irr)(f))).append(smerge(l.tail, r.tail)(f))
                    case _ => Stream(InCorrect(rvs.head, rvs.tail, smerge(ilr, irr)(f))).append(smerge(l.tail, r.tail)(f))
                  }
                case Correct(crq, crqs, crr) => (smerge(Stream(l.head), r.tail)(f)).append(smerge(l.tail, Stream(cnode(crq, crqs, crr)))(f))
                case it: ITrunk[A] => Stream(inode(ilq, ilqs, smerge(ilr, it.result).append(r.tail)))
                case ct: CTrunk[A] => Stream(inode(ilq, ilqs, ilr)).append(ct.result).append(r.tail)
              }
            }
          case Correct(clq, clqs, clr) => r match {
            case Stream.Empty => l
            case _ => r.head match {
              case i: InCorrect[A] => (smerge(l.tail, Stream(i))(f)).append(smerge(Stream(l.head), r.tail)(f))
              case Correct(crq, crqs, crr) =>
                //rootValues
                val rvs = f(Stream.cons(clq, clqs), Stream.cons(crq, crqs))
                rvs match {
                  case Stream.Empty => (smerge(l.tail, r.tail)(f)).append(Stream(ctrunk(smerge(l.head.result, crr)(f))))
                  case _ => (smerge(l.tail, r.tail)(f)).append(Stream(cnode(rvs.head, rvs.tail, smerge(l.head.result, crr)(f))))
                }
              case it: ITrunk[A] => smerge(it.result, Stream.cons(cnode(clq, clqs, clr), r.tail))
              case ct: CTrunk[A] => Stream(cnode(clq, clqs, smerge(ct.result, clr))).append(smerge(l.tail, r.tail))
            }
          }
          case lit: ITrunk[A] => r match {
            case Stream.Empty => l
            case _ => r.head match {
              case InCorrect(irq, irqs, irr) =>
                Stream(inode(irq, irqs, smerge(lit.result, irr))).append(smerge(l.tail, r.tail))
              case Correct(crq, crqs, crr) =>
                Stream(cnode(crq, crqs, smerge(lit.result, crr))).append(smerge(l.tail, r.tail))
              case rit: ITrunk[A] =>
                Stream(itrunk((smerge(lit.result, rit.result)))).append(smerge(l.tail, r.tail))
              case rct: CTrunk[A] =>
                Stream(lit, rct).append(smerge(l.tail, r.tail))
            }
          }
          case lct: CTrunk[A] => r match {
            case Stream.Empty => l
            case _ => r.head match {
              case InCorrect(irq, irqs, irr) =>
                Stream(inode(irq, irqs, smerge(irr, lct.result))).append(smerge(l.tail, r.tail))
              case Correct(crq, crqs, crr) =>
                Stream(cnode(crq, crqs, smerge(lct.result, crr))).append(smerge(l.tail, r.tail))
              case rit: ITrunk[A] =>
                Stream(rit, lct).append(smerge(l.tail, r.tail))
              case rct: CTrunk[A] =>
                smerge(lct.result, rct.result)
            }
          }
        }
      }
    }

    def path[A](p: Pool[A]): (Stream[A], String) = (p.rootValues, p.show)

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

    def updateR[A, B >: A](a: Answer[B]) = updateResult[A, B](a.r)(a.q) _

    def drop[A, B >: A](a: B)(f: A => Boolean)(s: Stream[A]): Either[Stream[B], Stream[B]] = s match {
      case h #:: t if f(h) => Right(t)
      case _ => Left(s)
    }

    def dropA[A, B >: A](a: B) = drop[A, B](a)(_ == a) _

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