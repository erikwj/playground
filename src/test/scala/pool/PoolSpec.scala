package pool

import org.specs2.mutable.Specification
import scalaz._
import Scalaz._
import std.stream.{ streamInstance, streamMonoid }
import pool.ResultItems.Answer
import pool.ResultItems.Correct
import pool.ResultItems.InCorrect
import pool.ResultItems.Pool

object PoolSpec extends Specification {

  import Pool._

  //First round of answers
  val a1 = Answer(1, false)
  val a2 = Answer(2, true)
  val a3 = Answer(3, false)
  val a4 = Answer(4, true)
  val a5 = Answer(5, true)
  val a6 = Answer(6, true)
  //Second round of answers
  val a1_2 = Answer(1, true)
  val a2_2 = Answer(2, true)
  val a3_2 = Answer(3, false)
  val a4_2 = Answer(4, false)
  val a5_2 = Answer(5, true)
  val a6_2 = Answer(6, false)

  //State transitions State0 + Answer1 => State1
  val s0 = inode(1, Stream(2, 3, 4, 5, 6), Stream.Empty)
  val s1 = inode(2, Stream(3, 4, 5, 6), Stream(ileaf(1)))
  val s2 = inode(3, Stream(4, 5, 6), Stream(ileaf(1), cleaf(2)))
  val s3 = inode(4, Stream(5, 6), Stream(inode(1, Stream(3), Stream.Empty), cleaf(2)))
  val s4 = inode(5, Stream(6), Stream(inode(1, Stream(3), Stream.Empty), cnode(2, Stream(4), Stream.Empty)))
  val s5 = inode(6, Stream.Empty, Stream(inode(1, Stream(3), Stream.Empty), cnode(2, Stream(4, 5), Stream.Empty)))
  val s6 = itrunk(Stream(inode(1, Stream(3), Stream.Empty), cnode(2, Stream(4, 5, 6), Stream.Empty)))

  //State transitions State6 + Answer1_2 => State1_2
  val s1_2 = itrunk(Stream(inode(3, Stream.Empty, Stream(cleaf(1))), cnode(2, Stream(4, 5, 6), Stream.Empty)))
  val s2_2 = itrunk(Stream(itrunk(Stream(ileaf(3), cleaf(1))), cnode(2, Stream(4, 5, 6), Stream.Empty)))
  val s3_2 = itrunk(Stream(itrunk(Stream(ileaf(3), cleaf(1))), cnode(4, Stream(5, 6), Stream(cleaf(2)))))
  val s4_2 = itrunk(Stream(itrunk(Stream(ileaf(3), cleaf(1))), cnode(5, Stream(6), Stream(ileaf(4), cleaf(2)))))
  val s5_2 = itrunk(Stream(itrunk(Stream(ileaf(3), cleaf(1))), cnode(6, Stream.Empty, Stream(ileaf(4), cnode(2, Stream(5), Stream.Empty)))))
  val s6_2 = itrunk(Stream(itrunk(Stream(ileaf(3), cleaf(1))), ctrunk(Stream(inode(4, Stream(6), Stream.Empty), cnode(2, Stream(5), Stream.Empty)))))

  "show should work" in {

    val s1 = inode(1, Stream(2), Stream.Empty)
    val s2 = ileaf(4)
    val s3 = cnode(1, Stream(2), Stream.Empty)
    val s4 = cleaf(4)
    val s5 = itrunk(Stream(inode(3, Stream.Empty, Stream(cleaf(1))), cnode(2, Stream(4, 5, 6), Stream.Empty)))
    val s6 = ctrunk(Stream(itrunk(Stream(ileaf(3), cleaf(1))), cnode(2, Stream(4, 5, 6), Stream.Empty)))

    s1.show must_== "I"
    s2.show must_== "I"
    s3.show must_== "C"
    s4.show must_== "C"
    s5.show must_== "I"
    s6.show must_== "C"

  }

  //Stream merge => merge of the results
  "smerge should work" in {
    implicit def merger[A](a: Stream[A], b: Stream[A]): Stream[A] = a.append(b)

    val s1 = Stream(inode(1, Stream(2), Stream.Empty), cleaf(3))
    val s2 = Stream(ileaf(4))
    val s3 = Stream(cleaf(14))

    //
    val m = Pool.smerge(s1, s2)
    val m2 = Pool.smerge(s2, s1)
    val m3 = Pool.smerge(s1, s3)
    val m4 = Pool.smerge(s3, s1)
    val m5 = Pool.smerge(s2, s3)
    val m6 = Pool.smerge(s3, s2)

    m must_== Stream(inode(1, Stream(2, 4), Stream.Empty), cleaf(3))
    m2 must_== Stream(inode(4, Stream(1, 2), Stream.Empty), cleaf(3))
    m3 must_== Stream(inode(1, Stream(2), Stream.Empty), cnode(3, Stream(14), Stream.Empty))
    m4 must_== Stream(inode(1, Stream(2), Stream.Empty), cnode(14, Stream(3), Stream.Empty))
    m5 must_== Stream(ileaf(4), cleaf(14))
    m6 must_== Stream(ileaf(4), cleaf(14))

  }

  "merge should work" in {
    implicit def merger[A](a: Stream[A], b: Stream[A]): Stream[A] = a.append(b)

    val s1 = inode(1, Stream(2), Stream.Empty)
    val s2 = ileaf(4)
    val s2string = ileaf("4")
    val s3 = cleaf(14)

    /**
     *                        left
     *                        /  \
     *                       /    \
     *                      /\    /\
     *                     / 2,4 5  6
     *                    /\
     *                   1  3
     */
    val left = itrunk(Stream(itrunk(Stream(itrunk(Stream(ileaf(1), cleaf(3))), cleafs(2, Stream(4)))), ctrunk(Stream(ileaf(5), cleaf(6)))))

    /**
     *                        right
     *                        /  \
     *                       /    \
     *                      /\    10,11
     *                     8  9
     */
    val right = itrunk(Stream(itrunk(Stream(ileaf(8), cleaf(9))), cleafs(10, Stream(11))))

    /**
     *                        result
     *                        /     \
     *                       /      10,11
     *                      / \      /\
     *                     8 2,4,9  5  6
     *                    /\
     *                   1  3
     */
    val result = itrunk(Stream(
      itrunk(Stream(
        inode(8, Stream.Empty, Stream(
          ileaf(1), cleaf(3))),
        cleafs(2, Stream(4, 9)))),
      cnode(10, Stream(11), Stream(
        ileaf(5), cleaf(6)))))

    //
    val m = Pool.merge(s1, s2)

    //TODO: how to avoid this
    //			  val mstring = Pool.merge[Int](s1, s2string)

    val m2 = Pool.merge(s2, s1)
    val m3 = Pool.merge(s1, s3)
    val m4 = Pool.merge(s3, s1)
    val m5 = Pool.merge(s2, s3)
    val m6 = Pool.merge(s3, s2)
    val m7 = Pool.merge(left, right)

    m must_== inode(1, Stream(2, 4), Stream.Empty)
    m2 must_== inode(4, Stream(1, 2), Stream.Empty)
    m3 must_== itrunk(Stream(inode(1, Stream(2), Stream.Empty), cleaf(14)))
    m4 must_== itrunk(Stream(inode(1, Stream(2), Stream.Empty), cleaf(14)))
    m5 must_== itrunk(Stream(ileaf(4), cleaf(14)))
    m6 must_== itrunk(Stream(ileaf(4), cleaf(14)))
    m7 must_== result

    //    val buf = new collection.mutable.ListBuffer[String]
    result.nodeMap("")(path(_))(_ + _).flatten.filter(p => !(p._1).isEmpty) must_==
      Stream(
        (Stream(8), "III"),
        (Stream(1), "IIII"),
        (Stream(3), "IIIC"),
        (Stream(2, 4, 9), "IIC"),
        (Stream(10, 11), "IC"),
        (Stream(5), "ICI"),
        (Stream(6), "ICC"))

  }

  "map should work" in {
    def add1(x: Int): Int = x + 1
    val s0 = inode(1, Stream(2, 3, 4, 5, 6), Stream.Empty)
    val s1 = inode(2, Stream(3, 4, 5, 6), Stream(ileaf(1)))
    val s2 = inode(3, Stream(4, 5, 6), Stream(ileaf(1), cleaf(2)))

    s0 map add1 must_== inode(2, Stream(3, 4, 5, 6, 7), Stream.Empty)
    s1 map add1 must_== inode(3, Stream(4, 5, 6, 7), Stream(ileaf(2)))
    s2 map add1 must_== inode(4, Stream(5, 6, 7), Stream(ileaf(2), cleaf(3)))
    s6_2 map add1 must_== itrunk(Stream(itrunk(Stream(ileaf(4), cleaf(2))), ctrunk(Stream(inode(5, Stream(7), Stream.Empty), cnode(3, Stream(6), Stream.Empty)))))
  }

  "nodeMap should work" in {
    s0.nodeMap("")(path(_))((x: String, y: String) => x + y) must_== inode((Stream(1, 2, 3, 4, 5, 6), "I"), Stream.Empty, Stream.Empty)
    s1.nodeMap("")(path(_))((x: String, y: String) => x + y) must_== inode((Stream(2, 3, 4, 5, 6), "I"), Stream.Empty, Stream(ileaf((Stream(1), "II"))))
    s1.nodeMap("")(path(_))((x: String, y: String) => x + y).flatten must_== Stream((Stream(2, 3, 4, 5, 6), "I"), (Stream(1), "II"))
  }

  "cobind should work" in {
    s0.cobind(path(_)) must_== inode((Stream(1, 2, 3, 4, 5, 6), "I"), Stream.Empty, Stream.Empty)
    s6.cobind(path(_)) must_== inode((Stream.Empty, "I"), Stream.Empty, Stream(ileaf((Stream(1, 3), "I")), cleaf((Stream(2, 4, 5, 6), "C"))))
  }

  "flattten should work" in {
    s0.flatten must_== Stream(1, 2, 3, 4, 5, 6)
    s1.flatten must_== Stream(2, 3, 4, 5, 6, 1)
    s2.flatten must_== Stream(3, 4, 5, 6, 1, 2)
    s3.flatten must_== Stream(4, 5, 6, 1, 3, 2)
    s4.flatten must_== Stream(5, 6, 1, 3, 2, 4)
    s5.flatten must_== Stream(6, 1, 3, 2, 4, 5)
    s6.flatten must_== Stream(1, 3, 2, 4, 5, 6)
    s1_2.flatten must_== Stream(3, 1, 2, 4, 5, 6)
    s2_2.flatten must_== Stream(3, 1, 2, 4, 5, 6)
  }

  "foldRight should work" in {
    val estream: Stream[Int] = Stream.Empty
    val fr = s0.foldRight(estream)((a, b) => Stream.cons(a, b))
    val fr1 = s1.foldRight(estream)((a, b) => Stream.cons(a, b))
    val fr2 = s2.foldRight(estream)((a, b) => Stream.cons(a, b))
    val fr3 = s3.foldRight(estream)((a, b) => Stream.cons(a, b))
    val fr62 = s6_2.foldRight(estream)(Stream.cons(_, _))
    fr must_== Stream(1, 2, 3, 4, 5, 6)
    fr1 must_== Stream(2, 3, 4, 5, 6, 1)
    fr2 must_== Stream(3, 4, 5, 6, 1, 2)
    fr3 must_== Stream(4, 5, 6, 1, 3, 2)
    fr62 must_== Stream(3, 1, 4, 6, 2, 5)
  }

  "levels should work" in {
    s2.levels must_== Stream(Stream(Stream(3, 4, 5, 6)), Stream(Stream(1), Stream(2)))
  }

  "nextPool should work" in {
    s0.nextPool.toList must_== List(1, 2, 3, 4, 5, 6)
    s1.nextPool.toList must_== List(2, 3, 4, 5, 6)
    s2.nextPool.toList must_== List(3, 4, 5, 6)
    s3.nextPool.toList must_== List(4, 5, 6)
  }

  "next should work" in {
    s0.next must_== 1
    s1.next must_== 2
    s2.next must_== 3
    s3.next must_== 4
  }

  "update should work" in {
    s1 must_== s0.update(a1)
    s2 must_== s1.update(a2)
    s3 must_== s2.update(a3)
    s4 must_== s3.update(a4)
    s5 must_== s4.update(a5)
    s6 must_== s5.update(a6)
    s1_2 must_== s6.update(a1_2)
    s2_2 must_== s1_2.update(a3_2)
    s3_2 must_== s2_2.update(a2_2)
    s4_2 must_== s3_2.update(a4_2)
    s5_2 must_== s4_2.update(a5_2)
    s6_2 must_== s5_2.update(a6_2)
  }

  "depth should work" in {
    s0.depth must_== 0
    s2.depth must_== 1
    s6_2.depth must_== 2

  }
  "countInCorrect should work" in {
    countInCorrect("ICCI") must_== Some(2)
    countCorrect("ICCI") must_== Some(2)
    countInCorrect("ICCCCCCCCI") must_== Some(2)
    countCorrect("ICCCCCCCCI") must_== Some(8)
    countInCorrect("II") must_== Some(2)
    countCorrect("II") must_== None
    countInCorrect("I") must_== Some(1)
    countInCorrect("CC") must_== None

  }

  "countSequence should work" in {
    //    def countCorrectSequence(z: String)(acc: Int)(out: Int)(s: List[Char])
    countLongestCorrect("ICCCCCCCCI") must_== 8
    countLongestCorrect("ICCCCICCCCI") must_== 4
    countLongestCorrect("ICCCCCICCCI") must_== 5
    countLongestCorrect("ICCCICCCCCI") must_== 5
    countLongestInCorrect("ICCCICCCCCI") must_== 1
    countLongestInCorrect("IIICCCICCCCCI") must_== 3
  }

  //
  //  "leafs should work" in {
  ////    s0.paths.toList must_== List((Stream(1, 2, 3, 4, 5, 6), "<I>"))
  ////    s1.paths.toList must_== List((Stream(2, 3, 4, 5, 6), "<I>"), (Stream(1), "<I>-<I>"))
  ////    s1a.paths.toList must_== List((Stream(2, 3, 4, 5, 6), "<I>"), (Stream(1), "<I>-<C>"))
  ////    s2.paths.toList must_== List((Stream(3, 4, 5, 6), "<I>"), (Stream(1), "<I>-<I>"), (Stream(2), "<I>-<C>"))
  //     		s7.leafs.toList must_==  List((Stream(1),"<I>"), (Stream(3),"<C>"), (Stream(2, 4, 5, 6),"<C>"))
  //    //    s1a.paths.toList must_== "bla"
  //  }

}

