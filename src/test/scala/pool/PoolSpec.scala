package pool

import org.specs2.mutable.Specification
import scalaz._
import Scalaz._
import std.stream.{ streamInstance, streamMonoid }
import pool.ResultItems.Answer
import pool.ResultItems.Correct
import pool.ResultItems.InCorrect
import pool.ResultItems.Pool
import scalaz.std.stream.streamMonoid

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

  //  val s2c = inode(Stream(3, 4, 5, 6), Stream(ileaf(Stream(1)), cleaf(Stream(2))))
  //  val s2 = inode(Stream(6), Stream(ileaf(Stream(1, 4)), cleaf(Stream(2, 3, 5))))
  //  val s6 = inode(Stream(6), Stream(ileaf(Stream(1, 4)), cleaf(Stream(2, 3, 5))))
  //  val s12 = inode(Stream(16), Stream(ileaf(Stream(11, 14)), cleaf(Stream(12, 13, 15))))
  //  val s6ac = inode(Stream(), Stream(ileaf(Stream(1, 4)), cleaf(Stream(2, 3, 5, 6))))
  //  val s7 = inode(Stream(), Stream(inode(Stream(), Stream(ileaf(Stream(1)), cleaf(Stream(3)))), cleaf(Stream(2, 4, 5, 6))))
  //  val s8 = inode(Stream(14), Stream(inode(Stream(), Stream(ileaf(Stream(1)), cleaf(Stream(3)))), cleaf(Stream(2, 4, 5, 6))))
  //  
  //  "mergeResult should work" in {
  //    implicit def merge[A](a:Stream[A],b:Stream[A]) = a.append(b)
  //    
  //    val s1 = Stream(ileaf(Stream(1, 2)), cleaf(Stream(3)))
  //    val s2 = Stream(ileaf(Stream(4)))
  //    val s3 = Stream(cleaf(Stream(4)))
  //    val s4 = Stream(cleaf(Stream(1, 2)), ileaf(Stream(3)))
  //    val s02 = Stream(inode(Stream(6), Stream(ileaf(Stream(1, 4)), cleaf(Stream(2, 3, 5)))))
  //    val s12 = Stream(inode(Stream(16), Stream(ileaf(Stream(11, 14)), cleaf(Stream(12, 13, 15)))))
  //    val s7 = Stream(inode(Stream(), Stream(inode(Stream(), Stream(ileaf(Stream(1)), cleaf(Stream(3)))), cleaf(Stream(2, 4, 5, 6)))))
  //    val s8 = Stream(ileaf(Stream(14)))
  //
  //    val m = Pool.mergeResult(s1, s2)
  //    val m2 = Pool.mergeResult(s2, s1)
  //    val m3 = Pool.mergeResult(s1, s3)
  //    val m4 = Pool.mergeResult(s3, s1)
  //    val m5 = Pool.mergeResult(s2, s3)
  //    val m6 = Pool.mergeResult(s3, s2)
  //    val m7 = Pool.mergeResult(s02, s12)
  //    val m8 = Pool.mergeResult(s7, s8)
  //    val m9 = Pool.mergeResult(s7, s02)
  //    
  //    m must_== Stream(ileaf(Stream(1, 2, 4)), cleaf(Stream(3)))
  //    m2 must_== Stream(ileaf(Stream(4, 1, 2)), cleaf(Stream(3)))
  //    m3 must_== Stream(ileaf(Stream(1, 2)), cleaf(Stream(3, 4)))
  //    m4 must_== Stream(ileaf(Stream(1, 2)), cleaf(Stream(4, 3)))
  //    m5 must_== Stream(ileaf(Stream(4)), cleaf(Stream(4)))
  //    m6 must_== Stream(ileaf(Stream(4)), cleaf(Stream(4)))
  //    m7 must_== Stream(inode(Stream(6, 16), Stream(ileaf(Stream(1, 4, 11, 14)), cleaf(Stream(2, 3, 5, 12, 13, 15)))))
  //    m8 must_== Stream(inode(Stream(14), Stream(inode(Stream(), Stream(ileaf(Stream(1)), cleaf(Stream(3)))), cleaf(Stream(2, 4, 5, 6)))))
  //    m9 must_== Stream(inode(Stream(6), Stream(inode(Stream(1,4), Stream(ileaf(Stream(1)), cleaf(Stream(3)))), cleaf(Stream(2, 4, 5, 6,2,3,5)))))
  //  }
  //

  "map should work" in {
    def add1(x: Int): Int = x + 1
    val s0 = inode(1, Stream(2, 3, 4, 5, 6), Stream.Empty)
    val s1 = inode(2, Stream(3, 4, 5, 6), Stream(ileaf(1)))
    val s2 = inode(3, Stream(4, 5, 6), Stream(ileaf(1), cleaf(2)))

    s0 map add1 must_== inode(2, Stream(3, 4, 5, 6, 7), Stream.Empty)
    s1 map add1 must_== inode(3, Stream(4, 5, 6, 7), Stream(ileaf(2)))
    s2 map add1 must_== inode(4, Stream(5, 6, 7), Stream(ileaf(2), cleaf(3)))
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
    val fr = s0.foldRight(estream)((a, b) => Stream.cons(a,b))
    fr must_== Stream(1, 2, 3, 4, 5, 6)
  }

  "levels should work" in {
    s2.levels must_== Stream(Stream(Stream(3, 4, 5, 6)), Stream(Stream(1), Stream(2)))
  }

  "next should work" in {
    s0.next.flatten.toList must_== List(1, 2, 3, 4, 5, 6)
    s1.next.flatten.toList must_== List(2, 3, 4, 5, 6, 1)
    s2.next.flatten.toList must_== List(3, 4, 5, 6, 1, 2)
    s3.next.flatten.toList must_== List(4, 5, 6, 1, 3, 2)
  }

  "addAnswer should work" in {
    Pool.updateResult(a1.r)(a1.q)(s0.result) must_== s1.result
    Pool.updateResult(a2.r)(a2.q)(s1.result) must_== s2.result
    Pool.updateResult(a3.r)(a3.q)(s2.result) must_== s3.result
    Pool.updateResult(a4.r)(a4.q)(s3.result) must_== s4.result
    Pool.updateResult(a5.r)(a5.q)(s4.result) must_== s5.result
    Pool.updateResult(a6.r)(a6.q)(s5.result) must_== s6.result
  }

  "update should work" in {
    val s01 = s0.update(a1)
    val s02 = s1.update(a2)
    val s03 = s2.update(a3)
    val s04 = s3.update(a4)
    val s05 = s4.update(a5)
    val s06 = s5.update(a6)
    val s01_2 = s6.update(a1_2)
    val s02_2 = s1_2.update(a3_2)
    val s03_2 = s2_2.update(a2_2)
    val s04_2 = s3_2.update(a4_2)
    val s05_2 = s4_2.update(a5_2)
    val s06_2 = s5_2.update(a6_2)

    s01 must_== s1
    s02 must_== s2
    s03 must_== s3
    s04 must_== s4
    s05 must_== s5
    s06 must_== s6
    s01_2 must_== s1_2
    s02_2 must_== s2_2
    s03_2 must_== s3_2
    s04_2 must_== s4_2
    s05_2 must_== s5_2
    s06_2 must_== s6_2
  }

  //  "depth should work" in {
  //    s0.depth must_== 1
  //    s2.depth must_== 2
  //    s7.depth must_== 3
  //  }
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

