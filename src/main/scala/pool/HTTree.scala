package pool

import scalaz._
import Scalaz._
import std.stream.{ streamInstance, streamMonoid }
import std.string.stringInstance

import scala.util.Either

/**
 * A HeadTailTree that contains a rootValue:A and possible a collections of A's as well.
 * The whole collection of A's is called rootValues
 * HeadTailTree => HTTree
 */
sealed abstract class HTTree[+A] {
  import HTTree._

  def result: Stream[HTTree[A]]

  def rootValues: Stream[A] = this match {
    case InCorrect(q, qs, _) => Stream.cons(q, qs)
    case Correct(q, qs, _) => Stream.cons(q, qs)
    case _ => Stream.Empty
  }

  def rootValue: A = rootValues.head

  /**
   * Update: updates a HTTree with an answers and returns a new HTTree
   */
  def update[B >: A](a: Answer[B]) = emap(dropA(a.q))(updateR(a))

  def fold[B](f: A => B)(g: (B, B) => B): B = result match {
    case Stream.Empty => f(rootValue)
    case Stream(l, r) => g(l.fold(f)(g), r.fold(f)(g))
  }

  def depth[B >: A]: Int =
    fold(x => 0)((d1, d2) => 1 + (d1 max d2))

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    Foldable[Stream].foldRight(flatten, z)(f)

  /**
   *
   *   Pre-order traversal inspired by scalaz.Tree
   *   TODO => remove init & last
   *
   */
  def flatten: Stream[A] = {
    def squish(pool: HTTree[A], xs: Stream[A]): Stream[A] = {
      val rvs = pool.rootValues
      rvs match {
        case Stream.Empty => (result map { _.flatten }).flatten

        case _ => rvs.init.append(Stream.cons(rvs.last, Foldable[Stream].foldr[HTTree[A], Stream[A]](pool.result, xs)(a => b => squish(a, b))))
      }
    }
    squish(this, Stream.Empty)
  }

  /**
   *
   *  Breadth-first traversal inspired by scalaz.Tree
   *
   */
  def levels: Stream[Stream[Stream[A]]] = {
    val f = (s: Stream[HTTree[A]]) => {
      Foldable[Stream].foldMap(s)((_: HTTree[A]).result)
    }
    Stream.iterate(Stream(this))(f) takeWhile (!_.isEmpty) map (_ map (_.rootValues))
  }

  //
  def nextPool = levels.flatten.filterNot(_.isEmpty).head
  //  def nextPool = levels takeWhile (!_.isEmpty) flatten
  def next = nextPool.head

  /**
   *
   * Map
   *
   */
  def map[B >: A](f: A => B): HTTree[B] = {
    lazy val resultMap = result map { _ map f }
    val rvs = rootValues map f
    construct(this, node(rvs, resultMap))
  }

  /**
   *
   * Map
   * def flatMap[B](f: A => Tree[B]): Tree[B] = {
   * val r: Tree[B] = f(rootLabel)
   * Tree.node(r.rootLabel, r.subForest #::: subForest.map(_.flatMap(f)))
   * }
   */
  def flatMap[B](f: A => HTTree[B]): HTTree[B] = {
    lazy val resultMap = result map { ht => f(ht.rootValue) }
    val rv = f(rootValue)
    construct(rv, node(rv.rootValues, resultMap))
  }

  /**
   *
   * EMap : Maps over all elements
   * Function f returns Either[rootValues,rootValues]:
   * - Right: if element is effected and rootValues (q,qs) are updated => Function g is applied to result
   * - Left if element is not effected
   *
   */

  def emap[B](f: Stream[A] => scala.util.Either[Stream[B], Stream[B]])(g: Stream[HTTree[B]] => Stream[HTTree[B]]): HTTree[B] = {

    //      def resultMap(r: Stream[HTTree[A]]) = r map { _.emap(f)(g) }
    lazy val resultMap = result map { _.emap(f)(g) }

    def rmap[B](e: scala.util.Either[Stream[B], Stream[B]])(rm: Stream[HTTree[B]])(g: Stream[HTTree[B]] => Stream[HTTree[B]])(n: (B, Stream[B], Stream[HTTree[B]]) => HTTree[B])(t: Stream[HTTree[B]] => HTTree[B]) = {
      def node(rvs: Stream[B], res: Stream[HTTree[B]]) = rvs match {
        case Stream.Empty => t(res)
        case _ => n(rvs.head, rvs.tail, res)
      }

      val r = e match {
        case Right(updated) => (updated, g(rm))
        case Left(nochange) => (nochange, rm)
      }
      node(r._1, r._2)
    }

    this match {
      case ITrunk(r) => itrunk(resultMap)
      case CTrunk(r) => ctrunk(resultMap)
      case InCorrect(_, _, _) => (rmap(f(rootValues))(resultMap)(g))(inode(_, _, _))(itrunk(_))
      case Correct(_, _, _) => (rmap(f(rootValues))(resultMap)(g))(cnode(_, _, _))(ctrunk(_))
    }
  }

  /**
   *
   * rVMap : Maps over all rootValues
   *
   */
  def rVMap[B >: A](f: Stream[A] => Stream[B]): HTTree[B] = construct(this, node(f(rootValues), result map { _.rVMap(f) }))

  /**
   *
   * HTTreeMap : Maps over all nodes
   *
   * /** Binds the given function across all the subtrees of this tree. */
   * def cobind[B](f: Tree[A] => B): Tree[B] = unfoldTree(this)(t => (f(t), () => t.subForest))
   *
   *
   */
  def foldMap[B, S, D](s: S)(f: HTTree[A] => (B, S))(g: (S, S) => S): HTTree[(B, S)] = {

      val r = f(this)
      lazy val sit = g(s, r._2)
      bind((r._1, sit), result map { (_.foldMap(sit)(f)(g)) })
  }


  def paths = this.foldMap("")(path(_))((x: String, y: String) => x + y)
  
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

  def cobind[B](f: HTTree[A] => B): HTTree[B] =
//    bind(f(this),result map { _ cobind f })
    this match {
      case i: ITrunk[A] => inode(f(i), Stream.Empty, result map { _ cobind f })
      case i: InCorrect[A] => inode(f(i), Stream.Empty, result map { _ cobind f })
      case c: CTrunk[A] => cnode(f(c), Stream.Empty, result map { _ cobind f })
      case c: Correct[A] => cnode(f(c), Stream.Empty, result map { _ cobind f })
    }

  def bind[B](h: B, r: Stream[HTTree[B]]): HTTree[B] =
    this match {
      case i: ITrunk[A] => inode(h, Stream.Empty, r map { p => bind(p.rootValue, p.result) })
      case i: InCorrect[A] => inode(h, Stream.Empty, r map { p => bind(p.rootValue, p.result) })
      case c: CTrunk[A] => cnode(h, Stream.Empty, r map { p => bind(p.rootValue, p.result) })
      case c: Correct[A] => cnode(h, Stream.Empty, r map { p => bind(p.rootValue, p.result) })
    }

  def show = this match {
    case x: InCorrect[A] => "I"
    case x: Correct[A] => "C"
    case x: ITrunk[A] => "I"
    case x: CTrunk[A] => "C"
  }
}

//Can be a leaf with empty result or a branch with some result
case class InCorrect[A](hd: A, tl: Stream[A], result: Stream[HTTree[A]]) extends HTTree[A]
case class Correct[A](hd: A, tl: Stream[A], result: Stream[HTTree[A]]) extends HTTree[A]
//Trunks have no leafs and thus no values
case class ITrunk[A](result: Stream[HTTree[A]]) extends HTTree[A]
case class CTrunk[A](result: Stream[HTTree[A]]) extends HTTree[A]

object HTTree {

  case class Answer[A](q: A, r: Boolean)

  //
  /** Construct a new Trunk with no values. */
  def itrunk[A](result: => Stream[HTTree[A]]): HTTree[A] = ITrunk(result)
  def ctrunk[A](result: => Stream[HTTree[A]]): HTTree[A] = CTrunk(result)
  //
  /** Construct a new HTTree node. */
  def ipool[A](root: Option[A], box: Stream[A], result: => Stream[HTTree[A]]): HTTree[A] = root match {
    case Some(r) => inode(r, box, result)
    case _ => itrunk(result)
  }

  def cpool[A](root: Option[A], box: Stream[A], result: => Stream[HTTree[A]]): HTTree[A] = root match {
    case Some(r) => cnode(r, box, result)
    case _ => ctrunk(result)
  }

  /** Construct a new HTTree node. */
  def inode[A](root: => A, box: => Stream[A], result: => Stream[HTTree[A]]): HTTree[A] = InCorrect(root, box, result)
  def cnode[A](root: => A, box: => Stream[A], result: => Stream[HTTree[A]]): HTTree[A] = Correct(root, box, result)

  /** Construct a HTTree node with one value and no children. */
  def ileaf[A](root: => A): HTTree[A] = inode(root, Stream.empty, Stream.empty)
  def cleaf[A](root: => A): HTTree[A] = cnode(root, Stream.empty, Stream.empty)
  //
  /** Construct a HTTree node with multiple values and no children. */
  def ileafs[A](root: => A, values: => Stream[A]): HTTree[A] = inode(root, values, Stream.empty)
  def cleafs[A](root: => A, values: => Stream[A]): HTTree[A] = cnode(root, values, Stream.empty)

  /**
   * Constructors
   */
  def node[A](rvs: Stream[A], res: => Stream[HTTree[A]]): Either[Stream[HTTree[A]], (A, Stream[A], Stream[HTTree[A]])] = rvs match {
    case Stream.Empty => Left(res)
    case _ => Right(rvs.head, rvs.tail, res)
  }

  def construct[A](p: HTTree[A], e: Either[Stream[HTTree[A]], (A, Stream[A], Stream[HTTree[A]])]): HTTree[A] = {
    (p, e) match {
      case (i: InCorrect[A], Left(r)) => itrunk(r)
      case (i: InCorrect[A], Right((q, qs, r))) => inode(q, qs, r)
      case (i: ITrunk[A], Left(r)) => itrunk(r)
      case (i: ITrunk[A], Right((q, qs, r))) => itrunk(r)
      case (c: Correct[A], Left(r)) => ctrunk(r)
      case (c: Correct[A], Right((q, qs, r))) => cnode(q, qs, r)
      case (c: CTrunk[A], Left(r)) => ctrunk(r)
      case (c: CTrunk[A], Right((q, qs, r))) => ctrunk(r)

    }
  }

  implicit def merge[A](l: HTTree[A], r: => HTTree[A])(implicit f: (Stream[A], Stream[A]) => Stream[A]): HTTree[A] = {
    lazy val resultMerge = smerge(l.result, r.result)
    def equalMerge(t: (A, Stream[A], Stream[HTTree[A]]) => HTTree[A]) = t(l.rootValue, f(l.rootValues.tail, r.rootValues), resultMerge)
    def nodeTrunkMerge(b: HTTree[A])(t: (A, Stream[A], Stream[HTTree[A]]) => HTTree[A]) = t(b.rootValue, b.rootValues.tail, resultMerge)

    (l, r) match {
      case (i: InCorrect[A], i2: InCorrect[A]) => equalMerge(inode(_, _, _))
      case (c: Correct[A], c2: Correct[A]) => equalMerge(cnode(_, _, _))
      case (i: InCorrect[A], c: Correct[A]) => itrunk(smerge(Stream(l), Stream(r)))
      case (c: Correct[A], i: InCorrect[A]) => itrunk(smerge(Stream(r), Stream(l)))
      case (i: InCorrect[A], _) => nodeTrunkMerge(i)(inode(_, _, _))
      case (c: Correct[A], _) => nodeTrunkMerge(c)(cnode(_, _, _))
      case (_, i: InCorrect[A]) => nodeTrunkMerge(i)(inode(_, _, _))
      case (_, c: Correct[A]) => nodeTrunkMerge(c)(cnode(_, _, _))
      case (_, _) => itrunk(resultMerge)
    }
  }

  //    def smerge[A](l: Stream[HTTree[A]], r: => Stream[HTTree[A]])(implicit f: (Stream[A], Stream[A]) => Stream[A]): Stream[HTTree[A]] = {
  //      l match {
  //        case Stream.Empty => r
  //        case _ =>
  //          r match {
  //            case Stream.Empty => l
  //            case _ =>
  //              lazy val resultMerge = smerge(l.tail, r.tail)
  //              Stream.cons(merge(l.head, r.head), resultMerge)
  //          }
  //      }
  //    }

  def smerge[A](l: Stream[HTTree[A]], r: => Stream[HTTree[A]])(implicit f: (Stream[A], Stream[A]) => Stream[A]): Stream[HTTree[A]] = {
    l match {
      case Stream.Empty => r
      case _ => l.head match {
        case i: InCorrect[A] =>
          r match {
            case Stream.Empty => l
            case _ =>
              lazy val resultMerge = smerge(l.head.result, r.head.result)
              r.head match {
                case i: InCorrect[A] =>
                  //rootValues
                  val rvs: Stream[A] = f(l.head.rootValues, r.head.rootValues)
                  Stream.cons(InCorrect(rvs.head, rvs.tail, resultMerge), smerge(l.tail, r.tail)(f))
                case c: Correct[A] => (smerge(Stream(l.head), r.tail)(f)).append(smerge(l.tail, Stream(c))(f))
                case it: ITrunk[A] => Stream(inode(i.rootValue, i.rootValues.tail, resultMerge.append(r.tail)))
                case ct: CTrunk[A] => Stream(inode(i.rootValue, i.rootValues.tail, i.result)).append(ct.result).append(r.tail)
              }
            //                Stream.cons(merge(l.head,r.head),Stream(merge(l.tail.head,r.tail.head)))
          }
        case c: Correct[A] => r match {
          case Stream.Empty => l
          case _ =>
            lazy val resultMerge = smerge(l.head.result, r.head.result)
            r.head match {
              case i: InCorrect[A] => (smerge(l.tail, Stream(r.head))(f)).append(smerge(Stream(l.head), r.tail)(f))
              case c: Correct[A] =>
                //rootValues
                val rvs = f(l.head.rootValues, r.head.rootValues)
                (smerge(l.tail, r.tail)(f)).append(Stream(cnode(rvs.head, rvs.tail, resultMerge)))
              case it: ITrunk[A] => smerge(it.result, Stream.cons(c, r.tail))
              case ct: CTrunk[A] => Stream.cons(cnode(c.rootValue, c.rootValues.tail, resultMerge), smerge(l.tail, r.tail))
            }
        }
        case lit: ITrunk[A] => r match {
          case Stream.Empty => l
          case _ =>
            lazy val resultMerge = smerge(l.head.result, r.head.result)
            r.head match {
              case i: InCorrect[A] =>
                Stream.cons(inode(r.head.rootValue, r.head.rootValues.tail, resultMerge), smerge(l.tail, r.tail))
              case c: Correct[A] =>
                Stream.cons(cnode(r.head.rootValue, r.head.rootValues.tail, resultMerge), smerge(l.tail, r.tail))
              case rit: ITrunk[A] =>
                Stream.cons(itrunk(resultMerge), smerge(l.tail, r.tail))
              case rct: CTrunk[A] =>
                Stream(l.head, r.head).append(smerge(l.tail, r.tail))
            }
        }
        case lct: CTrunk[A] => r match {
          case Stream.Empty => l
          case _ =>
            lazy val resultMerge = smerge(l.head.result, r.head.result)
            r.head match {
              case i: InCorrect[A] =>
                Stream.cons(inode(r.head.rootValue, r.head.rootValues.tail, resultMerge), smerge(l.tail, r.tail))
              case c: Correct[A] =>
                Stream.cons(cnode(r.head.rootValue, r.head.rootValues.tail, resultMerge), smerge(l.tail, r.tail))
              case rit: ITrunk[A] => Stream(r.head, l.head).append(smerge(l.tail, r.tail))
              case rct: CTrunk[A] => resultMerge
            }
        }
      }
    }
  }

  def path[A, K <: A](p: HTTree[K]): (Stream[K], String) = (p.rootValues, p.show)

  def countAnswers(s: String, k: Char) = {
    val m = s.groupBy(_.toChar).map(p => (p._1, p._2.length))
    m.get(k) match {
      case Some(v) => v
      case None => 0
    }
  }

  def countLongestSequence(z: Char)(acc: Int)(out: Int)(s: List[Char]): Int = s match {
    case Nil => out
    case h :: t if h == z =>
      val tot = acc + 1
      if (tot > out) countLongestSequence(z)(tot)(tot)(t)
      else countLongestSequence(z)(tot)(out)(t)
    case h :: t => countLongestSequence(z)(0)(out)(t)
  }

  def countCorrect(s: String) = countAnswers(s, 'C')
  def countInCorrect(s: String) = countAnswers(s, 'I')
  def countLongestCorrect(s: String): Int = countLongestSequence('C')(0)(0)(s.toList)
  def countLongestInCorrect(s: String): Int = countLongestSequence('I')(0)(0)(s.toList)

  def updateResult[A, B >: A](b1: Boolean)(a: B)(r: Stream[HTTree[B]]): Stream[HTTree[B]] = {
    def appendInCorrect[B >: A](p: HTTree[B], a: B): HTTree[B] =
      p match {
        case i: InCorrect[A] => InCorrect(i.hd, i.tl #::: Stream(a), i.result)
        case _ => ileaf(a)
      }

    def appendCorrect[B >: A](p: HTTree[B], a: B): HTTree[B] =
      p match {
        case c: Correct[A] => Correct(c.hd, c.tl #::: Stream(a), c.result)
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

