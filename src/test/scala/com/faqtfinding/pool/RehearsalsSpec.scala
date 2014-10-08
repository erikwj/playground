package com.faqtfinding.pool

import org.specs2.mutable.Specification
import com.faqtfinding.pool.Item._
import com.faqtfinding.pool.Rehearsals._
import com.faqtfinding.pool.Rehearsals.ItemSet._
import com.faqtfinding.pool.Answers._
import com.faqtfinding.pool.Question._
import scalaz._
import Scalaz._

object RehearsalsSpec extends Specification {
  import Rehearsal._
  import ItemResult._
  import RehearsalMock._

  "Rehearsal should " in {

    "have a non empty label " in {
      rhs.label must beAnInstanceOf[String]
      rhs.label must not be empty
      Rehearsal.rehearsal("", irs,Some(1)) must throwA[IllegalArgumentException]
    }

    "have a stopCriterium > 0 and < 10" in {
//      Rehearsal.rehearsal("incorrect", irs,Some(0)) must throwA[IllegalArgumentException]
//      Rehearsal.rehearsal("incorrect", irs,Some(10)) must throwA[IllegalArgumentException]
    todo
    }
    
    "create a looping instance of Rehearsal with None as stopCriterium (rehearse modus)" in {
      val loopRehearsal = Rehearsal.rehearsal("loopRehearsal", irs,None) 
      loopRehearsal must beAnInstanceOf[Rehearsal]
    	
 		  val iteration1 = loopRehearsal.update(a1).update(a2).update(a3).update(a4).update(a5).update(a6).update(a7)
 		  val iteration1_2f = loopRehearsal.update(a1).update(a2f).update(a3).update(a4).update(a5).update(a6).update(a7)
 		  val iteration1_a6 = loopRehearsal.update(a1).update(a2).update(a3).update(a4).update(a5).update(a6)

 		  "and the number of items in this instance should never shrink (i.e. filterFinishedItems is disabled)" in {
    	  iteration1.length must_== 7
    	  iteration1_a6.corrects.length must_== 6
    	  iteration1_2f.item must_== Some(i2)
    	}
      
      "that can be converted to nonLooping rehearsal (exam modus)" in {
        val nonLoopRehearsal = loopRehearsal.reset(Some(1))
        val iteration2 = nonLoopRehearsal.update(a1).update(a2).update(a3).update(a4).update(a5).update(a6).update(a7)
    	  iteration2.length must_== 0
      }
      
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
      rehearsal("Empty", Stream.Empty, Some(1)) must_== Rehearsal("Empty",Some(itemset(Stream(),Some(1))),Some(1))
    }
    
    "have an index function that returns position in itemlist" in {
      answered5.index must_== 5
    }

    "have the possibility to mix different item types in 1 rehearsal" in {
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
      val rhs_mcqis: Rehearsal = rehearsal("mcqis",mcqis, Some(2))
      rhs_mcqis.length must_== 1
    }
    
    "have a method to delete current item from a rehearsal" in {
      // Five is answered => Focus on 6
      // After deletion focus on 7
      // 1 2 3 4 5 6 7
      //           ^
      answered5.deleteCurrentItem.item must_== Some(i7)
    	
      // Six is answered => Focus on 7
      // After deletion focus on Nothing because next iteration is empty
      // 1 2 3 4 5 6 7
      //             ^
      answered6.deleteCurrentItem.item must_== None
    	answered7.deleteCurrentItem.item must_== None

    	// Six is answered => Focus on 7
      // After deletion focus on 1
      // 1 2 3 4 5 6 7
      //             ^
    	// 1 2 3 4 5 6 7
    	answered6_2.deleteCurrentItem.item must_== Some(i1)
    	
    	// Nothing is answered => Focus on 1
      // After deletion focus on 2
      // 1 2 3 4 5 6 7
      // ^
      //    	
    	// 2 3 4 5 6 7
    	// ^
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
      val stopCriterium = Some(2)
      
      val itemSet = Some(ItemSet.itemset(lefts, focus, rights,incorrects,corrects, stopCriterium))
    
      val nrhs = rehearsal("Created from separate input", itemSet,stopCriterium)
      nrhs.focus must_== Some(focus)
      nrhs.lefts must_== Some(lefts)
      nrhs.rights must_== Some(rights)
      nrhs.corrects must_== corrects
      nrhs.incorrects must_== incorrects
    
    }
        
    "generate a stream of lists of answerhistories" in {
      answered2.answerHistory must_== Stream(List((a1.q.iid -> a1.r)),List((a2.q.iid -> a2.r)))
    }
    
    "have a method that indicates if a rehearsal contains an item that is derived from a specific question" in {
      rhs.contains(mcq.qid) must beFalse
      rhs.contains(q1.qid) must beTrue
      mixedRehearsal.contains(mcq.qid) must beTrue
    }

  }
}