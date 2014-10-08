package com.faqtfinding.pool

import com.faqtfinding.pool.Item._
import com.faqtfinding.pool.Rehearsals._
import com.faqtfinding.pool.Rehearsals.ItemSet._
import com.faqtfinding.pool.Answers._
import com.faqtfinding.pool.Question._
import scalaz._
import Scalaz._

object RehearsalMock {
  import Rehearsal._
  import ItemResult._
  import RehearsalMock._
  
  val q1 = DICTQ(uuid, "Nederland", "Amsterdam")
  val q2 = DICTQ(uuid, "Belgie", "Brussel")
  val q3 = DICTQ(uuid, "Duitsland", "Berlijn")
  val q4 = DICTQ(uuid, "Luxemburg", "Luxemburg")
  val q5 = DICTQ(uuid, "Frankrijk", "Parijs")
  val q6 = DICTQ(uuid, "Spanje", "Madrid")
  val q7 = DICTQ(uuid, "Portugal", "Lissabon")
  val mcq = MCUQ(uuid, QuestionBody("WWII started in 1914"), Map(("Yes", true), ("No", false)), 1)
  val mcqi = mcq.item
  val qg = DICTQGroup(uuid, "Wat is de hoofdstad van", Some("Van welk land is dit de hoofdstad"), Stream(q1, q2, q3, q4, q5, q6, q7))
  val ditems = qg.items(false)
  val ditems2 = qg.items(true)
  val mixedItems = ditems ++ Stream(mcqi)

  //ItemResults
  val irs = ItemResult.toItemResult(ditems)
  val mirs = ItemResult.toItemResult(mixedItems)
  
  val irsA = irs.toArray
  val mirsA = mirs.toArray
  
  val rhs: Rehearsal = rehearsal("rhs1",irs, Some(1))
  val rhs2: Rehearsal = rehearsal("rhs2", irs, Some(2))
  val mixedRehearsal: Rehearsal = rehearsal("mixed rehearsal",mirs,Some(2))
  val ir1 = irsA.apply(0)
  val ir2 = irsA.apply(1)
  val ir3 = irsA.apply(2)
  val ir4 = irsA.apply(3)
  val ir5 = irsA.apply(4)
  val ir6 = irsA.apply(5)
  val ir7 = irsA.apply(6)
  
  //mixed Item
  val mir8 = mirsA.apply(7)
  
  val i1 = ir1.item
  val i2 = ir2.item
  val i3 = ir3.item
  val i4 = ir4.item
  val i5 = ir5.item
  val i6 = ir6.item
  val i7 = ir7.item
  val mi8 = mir8.item
  val a1 = AnswerResult(i1, Success(i1))
  val a1f = AnswerResult(i1, Failure("wrongAnswer"))
  val a2 = AnswerResult(i2, Success(i2))
  val a2f = AnswerResult(i2, Failure("wrongAnswer"))
  val a3 = AnswerResult(i3, Success(i3))
  val a3f = AnswerResult(i3, Failure("wrongAnswer"))
  val a4 = AnswerResult(i4, Success(i4))
  val a5 = AnswerResult(i5, Success(i5))
  val a6 = AnswerResult(i6, Success(i6))
  val a7 = AnswerResult(i7, Success(i7))
  
  val i1answered = ItemResult(i1, 1, List((a1.q.iid -> a1.r)))
  val i2answered = ItemResult(i2, 1, List((a2.q.iid -> a2.r)))
  val i2answeredf = ItemResult(i2, -1, List((a2f.q.iid -> a2f.r)))
  
  val answered2 = rhs.update(a1).update(a2)
  val answered2f = rhs.update(a1).update(a2f)
  val answered3 = answered2.update(a3)
  val answered4 = answered3.update(a4)
  val answered5 = answered4.update(a5)
  val answered6 = answered5.update(a6)
  val answered7 = answered6.update(a7)
  
  val answered6_2 = rhs2.update(a1).update(a2).update(a3).update(a4).update(a5).update(a6)
  
}