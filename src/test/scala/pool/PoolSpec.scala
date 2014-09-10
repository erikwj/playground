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

  //State 0
  val s0 = ileaf(Stream(1, 2, 3, 4, 5, 6))
  val a1 = Answer(Stream(1), false)
  val a2 = Answer(Stream(2), true)
  val a6 = Answer(Stream(6), true)
  //  
  //  //State 1 => updated State 0 by Answer
  //  val sa = s0.update(a1.r)(a1.q)
  //
  //  def emptyResult[A] = Stream.Empty
  //  
  val s1 = inode(Stream(2, 3, 4, 5, 6), Stream(ileaf(Stream(1))))
  val s1a = inode(Stream(2, 3, 4, 5, 6), Stream(Correct(Stream(1), Stream.Empty)))
  val s2b = inode(Stream(3, 4, 5, 6), Stream(ileaf(Stream(1)), cleaf(Stream(2))))
  val s2c = inode(Stream(3, 4, 5, 6), Stream(ileaf(Stream(1)), cleaf(Stream(2))))
  val s2 = inode(Stream(6), Stream(ileaf(Stream(1, 4)), cleaf(Stream(2, 3, 5))))
  val s6 = inode(Stream(6), Stream(ileaf(Stream(1, 4)), cleaf(Stream(2, 3, 5))))
  val s12 = inode(Stream(16), Stream(ileaf(Stream(11, 14)), cleaf(Stream(12, 13, 15))))
  val s6ac = inode(Stream(), Stream(ileaf(Stream(1, 4)), cleaf(Stream(2, 3, 5, 6))))
  val s7 = inode(Stream(), Stream(inode(Stream(), Stream(ileaf(Stream(1)), cleaf(Stream(3)))), cleaf(Stream(2, 4, 5, 6))))
  val s8 = inode(Stream(14), Stream(inode(Stream(), Stream(ileaf(Stream(1)), cleaf(Stream(3)))), cleaf(Stream(2, 4, 5, 6))))
  
  "mergeResult should work" in {
    val s1 = Stream(ileaf(Stream(1, 2)), cleaf(Stream(3)))
    val s2 = Stream(ileaf(Stream(4)))
    val s3 = Stream(cleaf(Stream(4)))
    val s4 = Stream(cleaf(Stream(1, 2)), ileaf(Stream(3)))
    val s02 = Stream(inode(Stream(6), Stream(ileaf(Stream(1, 4)), cleaf(Stream(2, 3, 5)))))
    val s12 = Stream(inode(Stream(16), Stream(ileaf(Stream(11, 14)), cleaf(Stream(12, 13, 15)))))
    val s7 = Stream(inode(Stream(), Stream(inode(Stream(), Stream(ileaf(Stream(1)), cleaf(Stream(3)))), cleaf(Stream(2, 4, 5, 6)))))
    val s8 = Stream(ileaf(Stream(14)))

    val m = Pool.mergeResult(s1, s2)((a, b) => a.append(b))
    val m2 = Pool.mergeResult(s2, s1)((a, b) => a.append(b))
    val m3 = Pool.mergeResult(s1, s3)((a, b) => a.append(b))
    val m4 = Pool.mergeResult(s3, s1)((a, b) => a.append(b))
    val m5 = Pool.mergeResult(s2, s3)((a, b) => a.append(b))
    val m6 = Pool.mergeResult(s3, s2)((a, b) => a.append(b))
    val m7 = Pool.mergeResult(s02, s12)((a, b) => a.append(b))
    val m8 = Pool.mergeResult(s7, s8)((a, b) => a.append(b))
    
    m must_== Stream(ileaf(Stream(1, 2, 4)), cleaf(Stream(3)))
    m2 must_== Stream(ileaf(Stream(4, 1, 2)), cleaf(Stream(3)))
    m3 must_== Stream(ileaf(Stream(1, 2)), cleaf(Stream(3, 4)))
    m4 must_== Stream(ileaf(Stream(1, 2)), cleaf(Stream(4, 3)))
    m5 must_== Stream(ileaf(Stream(4)), cleaf(Stream(4)))
    m6 must_== Stream(ileaf(Stream(4)), cleaf(Stream(4)))
    m7 must_== Stream(inode(Stream(6, 16), Stream(ileaf(Stream(1, 4, 11, 14)), cleaf(Stream(2, 3, 5, 12, 13, 15)))))
    m8 must_== Stream(inode(Stream(14), Stream(inode(Stream(), Stream(ileaf(Stream(1)), cleaf(Stream(3)))), cleaf(Stream(2, 4, 5, 6)))))
  }
  //
  //  "update should work" in {
  //    sa must_== s1
  //    s1.update(a2.r)(a2.q) must_== s2c
  //    s6.update(a6.r)(a6.q) must_== s6a
  //    s0.updateBranch(a1) must_== s1
  //  }
  //
  //  "levels should work" in {
  //    s1.levels must_== Stream(2,3,4,5,6,1)  
  //  }
  //  
  "map should work" in {
    def add1(x: Stream[Int]): Stream[Int] = x map { _ + 1 }
    def add1String(x: Stream[Int]): Stream[String] = x map { _.toString + "_1" }
    s0 map add1 must_== ileaf(Stream(2, 3, 4, 5, 6, 7))
    s6 map add1 must_== inode(Stream(7), Stream(ileaf(Stream(2, 5)), cleaf(Stream(3, 4, 6))))
    s2 map add1String must_== inode(Stream("6_1"), Stream(ileaf(Stream("1_1", "4_1")), cleaf(Stream("2_1", "3_1", "5_1"))))
  }

  "flatMap should work" in {
    //    def add1(x: Stream[Int]): Pool[Int] = InCorrect(x.head + 1,Stream.Empty) 
    //    s0 flatMap add1 must_== InCorrect(Stream(2, 3, 4, 5, 6, 7), Stream.Empty)
    todo
  }

  "flattten should work" in {
    s2.flatten must_== Stream(Stream(6), Stream(1, 4), Stream(2, 3, 5))
    s7.flatten.flatten must_== Stream(1, 3, 2, 4, 5, 6)
  }

  "foldMap should work" in {
    def add1(x: Stream[Int]): Stream[Int] = x map { _ + 1 }
    s0.foldMap(p => add1(p))
    todo
  }

  "levels should work" in {
    s2.levels must_== Stream(Stream(Stream(6)), Stream(Stream(1, 4), Stream(2, 3, 5)))
    s7.levels.flatten.flatten must_== Stream(2, 4, 5, 6, 1, 3)
  }

  "next should work" in {
    s0.next.flatten.toList must_== List(1, 2, 3, 4, 5, 6)
    s7.next.filter(!_.isEmpty).head.toList must_== List(2, 4, 5, 6)
    s8.next.flatten.toList must_== List(14, 2, 4, 5, 6, 1, 3)
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

