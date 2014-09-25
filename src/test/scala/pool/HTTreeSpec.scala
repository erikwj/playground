package pool

import org.specs2.mutable.Specification
import scalaz._
import Scalaz._
import std.stream.{ streamInstance, streamMonoid }
import pool.HTTree.AnswerResult

object HTTreeSpec extends Specification {

  import HTTree._
  import pool.Question._
  import pool.Answers._
  

  //First round of answers
  val a1 = AnswerResult(1, Failure("1"))
  val a2 = AnswerResult(2, Success(2))
  val a3 = AnswerResult(3, Failure("3"))
  val a4 = AnswerResult(4, Success(4))
  val a5 = AnswerResult(5, Success(5))
  val a6 = AnswerResult(6, Success(6))
  //Second round of answers
  val a1_2 = AnswerResult(1, Success(1))
  val a2_2 = AnswerResult(2, Success(2))
  val a3_2 = AnswerResult(3, Failure("3"))
  val a4_2 = AnswerResult(4, Failure("4"))
  val a5_2 = AnswerResult(5, Success(5))
  val a6_2 = AnswerResult(6, Failure("6"))

  //State transitions State0 + AnswerResult1 => State1
  val s0 = inode(1, Stream(2, 3, 4, 5, 6), Stream.Empty)
  val s1 = inode(2, Stream(3, 4, 5, 6), Stream(ileaf(1)))
  val s2 = inode(3, Stream(4, 5, 6), Stream(ileaf(1), cleaf(2)))
  val s3 = inode(4, Stream(5, 6), Stream(inode(1, Stream(3), Stream.Empty), cleaf(2)))
  val s4 = inode(5, Stream(6), Stream(inode(1, Stream(3), Stream.Empty), cnode(2, Stream(4), Stream.Empty)))
  val s5 = inode(6, Stream.Empty, Stream(inode(1, Stream(3), Stream.Empty), cnode(2, Stream(4, 5), Stream.Empty)))
  val s6 = itrunk(Stream(inode(1, Stream(3), Stream.Empty), cnode(2, Stream(4, 5, 6), Stream.Empty)))

  //State transitions State6 + AnswerResult1_2 => State1_2
  val s1_2 = itrunk(Stream(inode(3, Stream.Empty, Stream(cleaf(1))), cnode(2, Stream(4, 5, 6), Stream.Empty)))
  val s2_2 = itrunk(Stream(itrunk(Stream(ileaf(3), cleaf(1))), cnode(2, Stream(4, 5, 6), Stream.Empty)))
  val s3_2 = itrunk(Stream(itrunk(Stream(ileaf(3), cleaf(1))), cnode(4, Stream(5, 6), Stream(cleaf(2)))))
  val s4_2 = itrunk(Stream(itrunk(Stream(ileaf(3), cleaf(1))), cnode(5, Stream(6), Stream(ileaf(4), cleaf(2)))))
  val s5_2 = itrunk(Stream(itrunk(Stream(ileaf(3), cleaf(1))), cnode(6, Stream.Empty, Stream(ileaf(4), cnode(2, Stream(5), Stream.Empty)))))
  val s6_2 = itrunk(Stream(itrunk(Stream(ileaf(3), cleaf(1))), ctrunk(Stream(inode(4, Stream(6), Stream.Empty), cnode(2, Stream(5), Stream.Empty)))))

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
   *
   *  This is the result of a merge from left and right
   *
   */
  val result = itrunk(Stream(
    itrunk(Stream(
      inode(8, Stream.Empty, Stream(
        ileaf(1), cleaf(3))),
      cleafs(2, Stream(4, 9)))),
    cnode(10, Stream(11), Stream(
      ileaf(5), cleaf(6)))))

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
    val m = smerge(s1, s2)
    val m2 = smerge(s2, s1)
    val m3 = smerge(s1, s3)
    val m4 = smerge(s3, s1)
    val m5 = smerge(s2, s3)
    val m6 = smerge(s3, s2)

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

    //
    val m = merge(s1, s2)

    //TODO: how to avoid this String should not be accepted
    //			  val mstring = Pool.merge[Int](s1, s2string)

    val m2 = merge(s2, s1)
    val m3 = merge(s1, s3)
    val m4 = merge(s3, s1)
    val m5 = merge(s2, s3)
    val m6 = merge(s3, s2)
    val m7 = merge(left, right)

    m must_== inode(1, Stream(2, 4), Stream.Empty)
    m2 must_== inode(4, Stream(1, 2), Stream.Empty)
    m3 must_== itrunk(Stream(inode(1, Stream(2), Stream.Empty), cleaf(14)))
    m4 must_== itrunk(Stream(inode(1, Stream(2), Stream.Empty), cleaf(14)))
    m5 must_== itrunk(Stream(ileaf(4), cleaf(14)))
    m6 must_== itrunk(Stream(ileaf(4), cleaf(14)))
    m7 must_== result

    //    val buf = new collection.mutable.ListBuffer[String]

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

  "paths should work" in {
    s0.paths must_== Stream((Stream(1, 2, 3, 4, 5, 6), "I"))
    s1.paths must_== Stream((Stream(2, 3, 4, 5, 6), "I"), (Stream(1), "II"))

    result.paths must_==
      Stream(
        (Stream(8), "III"),
        (Stream(1), "IIII"),
        (Stream(3), "IIIC"),
        (Stream(2, 4, 9), "IIC"),
        (Stream(10, 11), "IC"),
        (Stream(5), "ICI"),
        (Stream(6), "ICC"))
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

    result.nextPool must_== Stream(10, 11)
    s0.nextPool.toList must_== List(1, 2, 3, 4, 5, 6)
    s1.nextPool.toList must_== List(2, 3, 4, 5, 6)
    s2.nextPool.toList must_== List(3, 4, 5, 6)
    s3.nextPool.toList must_== List(4, 5, 6)

    val s11 = result.update(AnswerResult(10, Success(10)))
    val s12 = s11.update(AnswerResult(11, Success(11)))
    s12.nextPool must_== Stream(8)
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
  "countInCorrect and countCorrect should work" in {
    countInCorrect("ICCI") must_== 2
    countCorrect("ICCI") must_== 2
    countInCorrect("ICCCCCCCCI") must_== 2
    countCorrect("ICCCCCCCCI") must_== 8
    countInCorrect("II") must_== 2
    countCorrect("II") must_== 0
    countInCorrect("I") must_== 1
    countInCorrect("CC") must_== 0
  }

  "countSequence should work" in {
    countLongestCorrect("ICCCCCCCCI") must_== 8
    countLongestCorrect("IIIIIIIIIIIIIIIIIIIIIIIIIIIIIII") must_== 0
    countLongestCorrect("ICCCCICCCCI") must_== 4
    countLongestCorrect("ICCCCCICCCI") must_== 5
    countLongestCorrect("ICCCICCCCCI") must_== 5
    countLongestInCorrect("ICCCICCCCCI") must_== 1
    countLongestInCorrect("IIICCCICCCCCI") must_== 3
  }

  "Mixed Item tree should work" in {
    val q1 = DICTI(ItemBody("Wat is de hoofdstad van", Some(List("Nederland"))), "Amsterdam")
    val q2 = DICTI(ItemBody("Wat is de hoofdstad van", Some(List("Belgie"))), "Brussel")
    val q3 = DICTI(ItemBody("Wat is de hoofdstad van", Some(List("Duitsland"))), "Berlijn")
    val q4 = DICTI(ItemBody("Wat is de hoofdstad van", Some(List("Luxemburg"))), "Luxemburg")
    val q5 = DICTI(ItemBody("Wat is de hoofdstad van", Some(List("Frankrijk"))), "Parijs")
    val q6 = DICTI(ItemBody("Wat is de hoofdstad van", Some(List("Spanje"))), "Madrid")
    val q7 = DICTI(ItemBody("Wat is de hoofdstad van", Some(List("Portugal"))), "Lissabon")
    val cijferR = MCUI(ItemBody("Welk cijfer vult deze reeks aan", Some(List("1", "4", "9", "16"))), Map(("20", false), ("25", true), ("30", false), ("9", false)), 1)
   
    val s0:HTTree[Item] = ileafs(q1, Stream(q2, q3, q4, q5, q6, q7,cijferR))
    s0.next must_== q1
    s0.nextPool must_== Stream(q1, q2, q3, q4, q5, q6, q7,cijferR)

    val s1 = s0.update(AnswerResult(q1, q1.isCorrect(SingleAnswer("Amsterdam"))))
    s1.next must_== q2
    s1 must_== inode(q2, Stream(q3, q4, q5, q6, q7,cijferR), Stream(cleaf(q1)))

    val s2i = s1.update(AnswerResult(q2, q2.isCorrect(SingleAnswer("Brusels"))))
    s2i.next must_== q3
    s2i must_== inode(q3, Stream(q4, q5, q6, q7,cijferR), Stream(ileaf(q2), cleaf(q1)))

    val s2c = s1.update(AnswerResult(q2, q2.isCorrect(SingleAnswer("Brussel"))))
    s2c.next must_== q3
    s2c must_== inode(q3, Stream(q4, q5, q6, q7,cijferR), Stream(cleafs(q1, Stream(q2))))
    
//    val s2cs = s1.update(AnswerResult(q2, q2.isCorrect(SingleAnswer("brussel"))))
//    s2cs.next must_== q3
//    s2cs must_== inode(q3, Stream(q4, q5, q6, q7), Stream(cleafs(q1, Stream(q2))))

    s2i.paths must_== Stream((Stream(q3, q4, q5, q6, q7,cijferR), "I"), ((Stream(q2), "II")), ((Stream(q1), "IC")))
    s2c.paths must_== Stream((Stream(q3, q4, q5, q6, q7,cijferR), "I"), ((Stream(q1, q2), "IC")))
  }

}

