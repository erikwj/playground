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

    //    def construct = this match {
    //      case i: InCorrect[A] => (q:A,qs:Stream[A],r:Stream[Pool[A]]) => ipool(Some(q),qs,r)
    //      case c: Correct[A] => (q:A,qs:Stream[A],r:Stream[Pool[A]]) => cpool(Some(q),qs,r)
    //      case it: ITrunk[A] => (q:A,qs:Stream[A],r:Stream[Pool[A]]) => ipool(None,Stream.Empty,r)
    //      case ct: CTrunk[A] => (q:A,qs:Stream[A],r:Stream[Pool[A]]) => cpool(None,Stream.Empty,r)
    //    }

    /**
     * Update: updates a Pool with an answers and returns a new Pool
     */
    def update[B >: A](a: Answer[B]) = emap(dropA(a.q))(updateR(a))

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      Foldable[Stream].foldRight(flatten, z)(f)

    /**
     *
     *   Pre-order traversal inspired by scalaz.Tree
     *   TODO => remove init & last
     *
     */
    def flatten: Stream[A] = {
      def squish(pool: Pool[A], xs: Stream[A]): Stream[A] = {
        val rvs = pool.rootValues
        rvs match {
          case Stream.Empty => (result map { _.flatten }).flatten
          
          case _ => rvs.init.append(Stream.cons(rvs.last, Foldable[Stream].foldr[Pool[A], Stream[A]](pool.result, xs)(a => b => squish(a, b))))
        }
      }
      squish(this, Stream.Empty)
    }

    /**
     *
     *  Breadth-first traversal inspired by scalaz.Tree
     *
     */
    def levels = {
      val f = (s: Stream[Pool[A]]) => {
        Foldable[Stream].foldMap(s)((_: Pool[A]).result)
      }
      Stream.iterate(Stream(this))(f) takeWhile (!_.isEmpty) map (_ map (_.rootValues))
    }

    //
    def nextPool = levels.head.flatten
    def next = nextPool.head

    /**
     *
     * Map
     *
     */
    def map[B](f: A => B): Pool[B] = {

      lazy val resultMap = result map { _ map f }
      this match {
        case ITrunk(_) => itrunk(resultMap)
        case CTrunk(_) => ctrunk(resultMap)
        case InCorrect(_, _, _) => inode(f(rootValues.head), rootValues.tail map f, resultMap)
        case Correct(_, _, _) => cnode(f(rootValues.head), rootValues.tail map f, resultMap)
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

      //      def resultMap(r: Stream[Pool[A]]) = r map { _.emap(f)(g) }
      lazy val resultMap = result map { _.emap(f)(g) }

      def node(rvs: Stream[B], res: Stream[Pool[B]])(n: (B, Stream[B], Stream[Pool[B]]) => Pool[B])(t: Stream[Pool[B]] => Pool[B]) = rvs match {
        case Stream.Empty => t(res)
        case _ => n(rvs.head, rvs.tail, res)
      }

      this match {
        case ITrunk(r) => itrunk(resultMap)
        case CTrunk(r) => ctrunk(resultMap)
        case InCorrect(_, _, _) =>
          f(rootValues) match {
            case Right(updated) => node(updated, g(resultMap))(inode(_, _, _))(itrunk(_))
            case Left(nochange) => node(nochange, resultMap)(inode(_, _, _))(itrunk(_))
          }

        case Correct(_, _, _) =>
          f(rootValues) match {
            case Right(updated) => node(updated, g(resultMap))(cnode(_, _, _))(ctrunk(_))
            case Left(nochange) => node(nochange, resultMap)(cnode(_, _, _))(ctrunk(_))
          }
      }
    }

    /**
     *
     * PoolMap : Maps over all elements
     *
     * /** Binds the given function across all the subtrees of this tree. */
     * def cobind[B](f: Tree[A] => B): Tree[B] = unfoldTree(this)(t => (f(t), () => t.subForest))
     *
     *
     */
    def nodeMap[B, S, D](s: S)(f: Pool[A] => (B, S))(g: (S, S) => S): Pool[(B, S)] = {

      def trace[B](p: Pool[A])(st: S)(f: Pool[A] => (B, S))(fp: ((B, S), Stream[Pool[(B, S)]]) => Pool[(B, S)]) = {
        val r = f(p)
        lazy val sit = g(s, r._2)
        fp((r._1, sit), p.result map { (_.nodeMap(sit)(f)(g)) })
      }

      this match {
        case i: ITrunk[A] => trace(i)(s)(f)(inode(_, Stream.Empty, _))
        case i: InCorrect[A] => trace(i)(s)(f)(inode(_, Stream.Empty, _))
        case c: CTrunk[A] => trace(c)(s)(f)(cnode(_, Stream.Empty, _))
        case c: Correct[A] => trace(c)(s)(f)(cnode(_, Stream.Empty, _))
      }
    }

    /**
     *
     * should be used to simplify nodeMap
     *
     * /** Binds the given function across all the subtrees of this tree. */
     * def cobind[B](f: Tree[A] => B): Tree[B] = unfoldTree(this)(t => (f(t), () => t.subForest))
     *
     * def unfoldForest[A, B](s: Stream[A])(f: A => (B, () => Stream[A])): Stream[Tree[B]] =
     *   s.map(unfoldTree(_)(f))
     *
     * def unfoldTree[A, B](v: A)(f: A => (B, () => Stream[A])): Tree[B] =
     *   f(v) match {
     *     case (a, bs) => node(a, unfoldForest(bs.apply())(f))
     *   }
     *
     */
    def cobind[B](f: Pool[A] => B): Pool[B] =
      this match {
        case i: ITrunk[A] => inode(f(i), Stream.Empty, result map { _ cobind f })
        case i: InCorrect[A] => inode(f(i), Stream.Empty, result map { _ cobind f })
        case c: CTrunk[A] => cnode(f(c), Stream.Empty, result map { _ cobind f })
        case c: Correct[A] => cnode(f(c), Stream.Empty, result map { _ cobind f })
      }

    
    
    
    
    
    
    
    def show = this match {
      case x: InCorrect[A] => "I"
      case x: Correct[A] => "C"
      case x: ITrunk[A] => "I"
      case x: CTrunk[A] => "C"
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
    /** Construct a new Pool node. */
    def ipool[A](root: Option[A], box: Stream[A], result: => Stream[Pool[A]]): Pool[A] = root match {
      case Some(r) => inode(r, box, result)
      case _ => itrunk(result)
    }

    def cpool[A](root: Option[A], box: Stream[A], result: => Stream[Pool[A]]): Pool[A] = root match {
      case Some(r) => cnode(r, box, result)
      case _ => ctrunk(result)
    }

    /** Construct a new Pool node. */
    def inode[A](root: => A, box: => Stream[A], result: => Stream[Pool[A]]): Pool[A] = InCorrect(root, box, result)
    def cnode[A](root: => A, box: => Stream[A], result: => Stream[Pool[A]]): Pool[A] = Correct(root, box, result)

    /** Construct a Pool node with one value and no children. */
    def ileaf[A](root: => A): Pool[A] = inode(root, Stream.empty, Stream.empty)
    def cleaf[A](root: => A): Pool[A] = cnode(root, Stream.empty, Stream.empty)
    //
    /** Construct a Pool node with multiple values and no children. */
    def ileafs[A](root: => A, values: => Stream[A]): Pool[A] = inode(root, values, Stream.empty)
    def cleafs[A](root: => A, values: => Stream[A]): Pool[A] = cnode(root, values, Stream.empty)
    //
    //        def apply[A](q: => A, qs: => Stream[A]): Pool[A] = incorrect(q,qs, Stream.Empty)

    def merge[A](l: Pool[A], r: => Pool[A])(implicit f: (Stream[A], Stream[A]) => Stream[A]): Pool[A] = {
      lazy val resultMerge = smerge(l.result, r.result)
      (l, r) match {
        case (i: InCorrect[A], i2: InCorrect[A]) => 
          inode(i.current, 
              i.box.append(Stream.cons(i2.current, i2.box)), 
              resultMerge)
        case (c: Correct[A], c2: Correct[A]) => 
          cnode(c.current, 
              c.box.append(Stream.cons(c2.current, c2.box)), 
              resultMerge)
        case (i: InCorrect[A], c: Correct[A]) => 
          itrunk(smerge(Stream(i), Stream(c)))
        case (c: Correct[A], i: InCorrect[A]) => 
          itrunk(smerge(Stream(i), Stream(c)))
        case (i: InCorrect[A], it: ITrunk[A]) => 
          inode(i.current, i.box, resultMerge)
        case (i: InCorrect[A], ct: CTrunk[A]) => 
          inode(i.current, i.box, resultMerge)
        case (c: Correct[A], it: ITrunk[A]) => 
          cnode(c.current, c.box, resultMerge)
        case (c: Correct[A], ct: CTrunk[A]) => 
          cnode(c.current, c.box, resultMerge)
        case (_, i: InCorrect[A]) => 
          inode(i.current, i.box, resultMerge)
        case (_, c: Correct[A]) => 
          cnode(c.current, c.box, resultMerge)
        case (_, _) => 
          itrunk(resultMerge)
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

}