package pool

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import pool.ResultItems.{ Answer, InCorrect, Correct, Result, Pool }

object PoolSpec extends Specification {

  //State 0
  val s0 = InCorrect(Stream(1, 2, 3, 4, 5, 6), Result(None, None))
  val a1 = Answer(1, false)
  val a2 = Answer(2, true)
  //State 1 => updated State 0 by Answer
  val sa = s0.update(a1)

  val s1 = InCorrect(Stream(2, 3, 4, 5, 6), Result(Some(InCorrect(Stream(1), Result(None, None))), None))
  val s2 = InCorrect(Stream(3, 4, 5, 6), Result(Some(InCorrect(Stream(1), Result(None, None))), Some(Correct(Stream(2), Result(None, None)))))
    val s7 = InCorrect(Stream(), Result(Some(InCorrect(Stream(3), Result(Some(InCorrect(Stream(1), Result(None, None))), None))), Some(Correct(Stream(2, 4, 5, 6), Result(None, None)))))

  "levels should work" in {
    sa must_== s1
    s1.update(a2) must_== s2
  }

  "map should work" in {
    def add1(x: Stream[Int]): Stream[Int] = x map { _ + 1 }
    def add1String(x: Stream[Int]): Stream[String] = x map { _.toString + "_1" }
    s0 map add1 must_== Some(InCorrect(Stream(2, 3, 4, 5, 6, 7), Result(None, None)))
    s2 map add1String must_== Some(InCorrect(Stream("3_1", "4_1", "5_1", "6_1"), Result(Some(InCorrect(Stream("1_1"), Result(None, None))), Some(Correct(Stream("2_1"), Result(None, None))))))
  }
  
  "next should work" in {
    s0.next.toList must_== List(1,2,3,4,5,6)
    s1.next.toList must_== List(2,3,4,5,6)
    s7.next.toList must_== List(3)
  }
  
  "depth should work" in {
    s0.depth must_== 1
    s2.depth must_== 2
    s7.depth must_== 3
  }

}

