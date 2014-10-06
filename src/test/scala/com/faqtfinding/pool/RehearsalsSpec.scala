package com.faqtfinding.pool

import org.specs2.mutable.Specification
import com.faqtfinding.pool.Item._
import com.faqtfinding.pool.Rehearsals._
import com.faqtfinding.pool.Answers._
import com.faqtfinding.pool.Question._
import scalaz._
import Scalaz._

object RehearsalsSpec extends Specification {
  
    val q1 = DICTQ(uuid, "Nederland", "Amsterdam")
  val q2 = DICTQ(uuid, "Belgie", "Brussel")
  val q3 = DICTQ(uuid, "Duitsland", "Berlijn")
  val q4 = DICTQ(uuid, "Luxemburg", "Luxemburg")
  val q5 = DICTQ(uuid, "Frankrijk", "Parijs")
  val q6 = DICTQ(uuid, "Spanje", "Madrid")
  val q7 = DICTQ(uuid, "Portugal", "Lissabon")

  val qg = DICTQGroup(uuid, "Wat is de hoofdstad van", Some("Van welk land is dit de hoofdstad"), Stream(q1, q2, q3, q4, q5, q6, q7))
    val ditems = qg.items(false)
    val ditems2 = qg.items(true)
  
  
   "Rehearsal should work" in {
    
    //ItemResults
    val irs = Item.toItemResult(ditems)
    val irsA = irs.toArray

    //ErrorHandling
    // val rhs0:Rehearsal = Rehearsal.rehearsal(irs,0)


    val rhs:Rehearsal = Rehearsal.rehearsal(irs,1)
    val rhs2:Rehearsal = Rehearsal.rehearsal(irs,2)
    val emptyRehearsal = Rehearsal.rehearsal(Stream.Empty,1)
    val ir1 = irsA.apply(0)
    val ir2 = irsA.apply(1)
    val ir3 = irsA.apply(2)
    val ir4 = irsA.apply(3)
    val ir5 = irsA.apply(4)
    val ir6 = irsA.apply(5)
    val ir7 = irsA.apply(6)
    val i1 = ir1.item
    val i2 = ir2.item
    val i3 = ir3.item
    val i4 = ir4.item
    val i5 = ir5.item
    val i6 = ir6.item
    val i7 = ir7.item
    val a1 = AnswerResult(i1, Success(i1))
    val a2 = AnswerResult(i2, Success(i2))
    val a2f = AnswerResult(i2, Failure("wrongAnswer"))
    val a3 = AnswerResult(i3, Success(i3))
    val a4 = AnswerResult(i4, Success(i4))
    val a5 = AnswerResult(i5, Success(i5))
    val a6 = AnswerResult(i6, Success(i6))
    val a7 = AnswerResult(i7, Success(i7))
  
    // rhs.nextC must_== Some(irs.tail.head)
    // rhs.itemResult must_== Some(irs.head)
    rhs.item must_== Some(i1)
    rhs.update(a1).focus must_== Some(ItemResult(i2,0,List()))
    rhs.update(a1).update(a2).focus must_== Some(ItemResult(i3,0,List()))
    val i1answered = ItemResult(i1,1,List((a1.q.iid -> a1.r)))
    val i2answered = ItemResult(i2,1,List((a2.q.iid -> a2.r)))
    val answered2 = rhs.update(a1).update(a2)
    val answered3 = answered2.update(a3)
    val answered4 = answered3.update(a4)
    val answered5 = answered4.update(a5)
    val answered6 = answered5.update(a6)
    val answered7 = answered6.update(a7)
    answered2.focus must_== Some(ItemResult(i3,0,List()))
    answered2.lefts must_== Some(Stream.Empty)
    answered2.rights must_== Some(Stream(i1answered,i2answered))
    // answered7.focus must_== Some(ItemResult(i1,1,List((a1.q.iid -> a1.r))))
    answered7.focus must_== None
    // answered7.isAnswered must_== false
    answered7.isAnswered must_== true
    answered7.isFinished must_== true

    rhs.item must_== Some(irs.head.item)
    emptyRehearsal must_== Rehearsal(None,1)

    //2 Correct items
    val answered7_2 = rhs2.update(a1).update(a2).update(a3).update(a4).update(a5).update(a6).update(a7)
    answered7_2.isFinished must_== false
    answered7_2.isAnswered must_== false
    answered7_2.item must_== rhs.item

    //only go to next if item in answer matches current item
    answered2.update(a1).focus must_== answered2.focus
  }
}