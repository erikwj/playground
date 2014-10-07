package com.faqtfinding.pool

import org.specs2.mutable.Specification
import com.faqtfinding.pool.Item._
import com.faqtfinding.pool.Rehearsals._
import com.faqtfinding.pool.Answers._
import com.faqtfinding.pool.Question._
import scalaz._
import Scalaz._

object RehearsalsSpec extends Specification {
  import Rehearsal._
  import ItemResult._

  val q1 = DICTQ(uuid, "Nederland", "Amsterdam")
  val q2 = DICTQ(uuid, "Belgie", "Brussel")
  val q3 = DICTQ(uuid, "Duitsland", "Berlijn")
  val q4 = DICTQ(uuid, "Luxemburg", "Luxemburg")
  val q5 = DICTQ(uuid, "Frankrijk", "Parijs")
  val q6 = DICTQ(uuid, "Spanje", "Madrid")
  val q7 = DICTQ(uuid, "Portugal", "Lissabon")
  val mcqi = MCUQ(uuid, QuestionBody("WWII started in 1914"), Map(("Yes", true), ("No", false)), 1).item

  val qg = DICTQGroup(uuid, "Wat is de hoofdstad van", Some("Van welk land is dit de hoofdstad"), Stream(q1, q2, q3, q4, q5, q6, q7))
  val ditems = qg.items(false)
  val ditems2 = qg.items(true)
  val mixedItems = ditems ++ Stream(mcqi)

  "Rehearsal should " in {

    //ItemResults
    val irs = ItemResult.toItemResult(ditems)
 		val mirs = ItemResult.toItemResult(mixedItems)
    
    val irsA = irs.toArray
    val mirsA = mirs.toArray

    val rhs: Rehearsal = rehearsal("rhs1",irs, 1)
    val rhs2: Rehearsal = rehearsal("rhs2", irs, 2)
    val mixedRehearsal: Rehearsal = rehearsal("mixed rehearsal",mirs,2)
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
      
    "have a non empty label " in {
      rhs.label must beAnInstanceOf[String]
      rhs.label must not be empty
      Rehearsal.rehearsal("", irs,1) must throwA[IllegalArgumentException]
    }

    "have a stopCriterium > 0 and < 10" in {
      Rehearsal.rehearsal("incorrect", irs,0) must throwA[IllegalArgumentException]
      Rehearsal.rehearsal("incorrect", irs,10) must throwA[IllegalArgumentException]
    }

    "have a check that only matching answers will update itemresults" in {
      rhs.item must_== Some(i1)
      rhs.update(a1).focus must_== Some(itemresult(i2))
      rhs.update(a2).item must_== rhs.item
    }
    
    "have a possibility to chain updates" in {
      rhs.update(a1).update(a2).focus must_== Some(itemresult(i3))
    }

    "have a focus method that returns current item that needs to be answered" in {
      answered2.focus must_== Some(itemresult(i3))
    }

    "have a lefts container that contains incorrect answers" in {
      answered2.incorrects must_== Stream.Empty
      answered2f.incorrects must_== Stream(i2answeredf)
    }
    
    "have a rights container that contains correct answers" in {
      answered2.corrects must_== Stream(i1answered, i2answered)
    }

    "have a stopCriterium that returns no new item if all items comply to stopCriterium" in {
      //atEnd
      //Call to focus results in None
      answered7.focus must_== None
      //Both answered and finished if number times finished correct equals stopCriterium
      answered7.isAnswered must beTrue
      answered7.isFinished must beTrue

      rhs.item must_== Some(irs.head.item)
      rehearsal("Empty", Stream.Empty, 1) must_== Rehearsal("Empty", None, 1)

    }
    
    "have an index function that returns position in itemlist" in {
      answered5.index must_== 5
    }

    "have the possibility to mix different item types in 1 rehearsal" in {
        //mixed answers should work
        val mixedRehearsal7Answered = mixedRehearsal.update(a1).update(a2).update(a3).update(a4).update(a5).update(a6).update(a7)
        mixedRehearsal7Answered.focus must_== Some(itemresult(mi8))
    }

    "merge incorrects and correctss after all items have been answered" in {
      val answeredLast_2CorrectAnswersNeeded = rhs.update(a1).update(a2f).update(a3).update(a4).update(a5).update(a6).update(a7)
        answeredLast_2CorrectAnswersNeeded.isFinished must beFalse
        answeredLast_2CorrectAnswersNeeded.isAnswered must beFalse

      "and filter out items that comply to stopCriterium" in {

        answeredLast_2CorrectAnswersNeeded.isFinished must beFalse
        //score = -1 + 1 = 0
        val lastAnswered = answeredLast_2CorrectAnswersNeeded.update(a2)
        lastAnswered.isFinished must beFalse
        
        lastAnswered.update(a2).isFinished must beTrue
      }

      "and put lefts before rights" in {
        answeredLast_2CorrectAnswersNeeded.item must_== Some(i2)
      }
    }
    
    "have a length method that returns the number of items currently in rehearsal" in {
      rhs.length must_== 7
      answered7.length must_== 0
    }

    "only contain unique items" in {
      val mcqis = ItemResult.toItemResult(Stream(mcqi,mcqi))
      val rhs_mcqis: Rehearsal = rehearsal("mcqis",mcqis, 2)
      rhs_mcqis.length must_== 1
    }
    
    "have a method to delete current item from a rehearsal" in {
      // Five is answered => Focus on 6
      // After deletion focus on 7
      // 1 2 3 4 5 6 7
      //           ^
      answered5.deleteCurrentItem.item must_== Some(i7)
 		  answered6.deleteCurrentItem.item must_== None
    	answered7.deleteCurrentItem.item must_== None
    	// 2 correct answers needed
    	answered6_2.deleteCurrentItem.item must_== Some(i1)
    	rhs.deleteCurrentItem.item must_== Some(i2)
    }
    
    
    "have a method to insert an item in a rehearsal" in {
      todo
    }
    
    "have a method to find an item in a rehearsal" in {
      todo
    }
    
    "have a method to reset a rehearsal" in {
      val reset5 = answered5.reset
      reset5.item must_== Some(i1)
      reset5.incorrects must_== Stream.Empty
      reset5.corrects must_== Stream.Empty
    }
    
    "have a method to create a rehearsal from lefts, focus, rights" in {
      val lefts = Stream(ir1,ir2)
      val focus = ir3
      val rights = Stream(ir4,ir5)
      val corrects = Stream(i1answered,i2answered)
      val incorrects = Stream.Empty
    
      val nrhs = rehearsal("Created from separate input", lefts,focus,rights,2)
      nrhs.focus must_== Some(focus)
      nrhs.lefts must_== Some(lefts)
      nrhs.rights must_== Some(rights)
    
    }

  }
}