package pool

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import pool.ResultItems.{ Answer, InCorrect, Correct, Pool }

object PoolSpec extends Specification {

  //State 0
  val s0 = InCorrect(Stream(1, 2, 3, 4, 5, 6), Stream.Empty)
  val a1 = Answer(Stream(1), false)
  val a2 = Answer(Stream(2), true)
  val a6 = Answer(Stream(6), true)
  //  
  //  //State 1 => updated State 0 by Answer
  //  val sa = s0.update(a1.r)(a1.q)
  //
  //  def emptyResult[A] = Stream.Empty
  //  
  val s1 = InCorrect(Stream(2, 3, 4, 5, 6), Stream(InCorrect(Stream(1), Stream.Empty)))
  val s1a = InCorrect(Stream(2, 3, 4, 5, 6), Stream(Correct(Stream(1), Stream.Empty)))
  val s2b = InCorrect(Stream(3, 4, 5, 6), Stream(InCorrect(Stream(1), Stream.Empty), Correct(Stream(2), Stream.Empty)))
  val s2c = InCorrect(Stream(3, 4, 5, 6), Stream(InCorrect(Stream(1), Stream.Empty), Correct(Stream(2), Stream.Empty)))
  val s2 = InCorrect(Stream(6), Stream(InCorrect(Stream(1, 4), Stream.Empty), Correct(Stream(2, 3, 5), Stream.Empty)))
  val s6 = InCorrect(Stream(6), Stream(InCorrect(Stream(1, 4), Stream.Empty), Correct(Stream(2, 3, 5), Stream.Empty)))
  val s6a = InCorrect(Stream(), Stream(InCorrect(Stream(1, 4), Stream.Empty), Correct(Stream(2, 3, 5, 6), Stream.Empty)))
  val s7 = InCorrect(Stream(), Stream(InCorrect(Stream(), Stream(InCorrect(Stream(1), Stream.Empty), Correct(Stream(3), Stream.Empty))), Correct(Stream(2, 4, 5, 6), Stream.Empty)))
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
    s0 map add1 must_== InCorrect(Stream(2, 3, 4, 5, 6, 7), Stream.Empty)
    s6 map add1 must_== InCorrect(Stream(7), Stream(InCorrect(Stream(2, 5), Stream.Empty), Correct(Stream(3, 4, 6), Stream.Empty)))
    s2 map add1String must_== InCorrect(Stream("6_1"), Stream(InCorrect(Stream("1_1", "4_1"), Stream.Empty), Correct(Stream("2_1", "3_1", "5_1"), Stream.Empty)))
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
    todo
  }

  "levels should work" in {
    s2.levels must_== Stream(Stream(Stream(6)), Stream(Stream(1, 4), Stream(2, 3, 5)))
    s7.levels.flatten.flatten must_== Stream(2, 4, 5, 6, 1, 3)
  }

  "next should work" in {
    s0.next.flatten.toList must_== List(1, 2, 3, 4, 5, 6)
    //    s1.next must_== Some(InCorrect(Stream(2, 3, 4, 5, 6), None, Some(Correct(Stream(1), None, None))))))
    //    s2.next.toList must_== List(6)
    s7.next.filter(!_.isEmpty).head.toList must_== List(2, 4, 5, 6)
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

