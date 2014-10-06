package com.faqtfinding.pool

import org.specs2.mutable.Specification
import com.faqtfinding.pool.HTTree.AnswerResult
import com.faqtfinding.pool.Item._
import com.faqtfinding.pool.Question._
import com.faqtfinding.pool.Answers._
import scalaz._
import Scalaz._

object QuestionsSpec extends Specification {
  import com.faqtfinding.pool.Question
  import com.faqtfinding.pool.Answers._
  import com.faqtfinding.pool.HTTree._
  import com.faqtfinding.pool.Item._

  val mc1 = MCUQ(uuid, QuestionBody("WWII started in 1914"), Map(("Yes", true), ("No", false)), 1)
  val mc1_i = mc1.item

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


  "TrueFalseQuestion => MultipleChoiceQuestion can be answered" in {
    mc1_i.isCorrect(MultipleAnswers(List("Yes"))) must_== Success(mc1_i)
    mc1_i.isCorrect(MultipleAnswers(List("No"))) must_== Failure("wrongAnswer")
    mc1_i.isCorrect(MultipleAnswers(List("Yes", "No"))) must_== Failure("wrongAnswer")
    mc1_i.isCorrect(MultipleAnswers(List("No", "Yes"))) must_== Failure("wrongAnswer")

    mc1.asHtml must_== """<div class="label">WWII started in 1914</div><div class="alternative-container"><div class="alternative">Yes</div><div class="correct">true</div><div class="alternative">No</div><div class="correct">false</div></div>"""
    mc1_i.asHtml must_== mc1.asHtml 

  }

  "Questions can have an empty form " in {
    val dictform = DICTQGroup.asHtml(None,None,Stream.Empty)
    dictform must_==  s"""<div class="instruction"></div>""" +
        s"""<div class="reverseInstruction"></div>""" +
        s"""<div class="item-container"><div class="body"><input type="text" class="form-control" id="question" placeholder=""></div><div class="answer"><input type="text" class="form-control" id="answer" placeholder=""></div></div>"""

    val mcoform = MCOQ.asHtml(None,None,0)
    mcoform must_==   s"""<div class="label"></div><div class="statement-container"><ul class="list-inline"><li><div class="statement"></div></li></ul></div>""" + 
    s"""<div class="alternative-container"><ol><li><div class="alternative"></div></li></ol></div>"""

    val mcuform = MCUQ.asHtml(None,None,0)
    mcuform must_==   s"""<div class="label"></div><div class="statement-container"><ul class="list-inline"><li><div class="statement"></div></li></ul></div>""" + 
    s"""<div class="alternative-container"><ul><li><div class="alternative"></div><div class="correct"></div></li></ul></div>"""
      
  }

  "DICTI can be answered" in {
    val dq = DICTI(uuid,QuestionBody("Wat is de hoofdstad van", Some(List("Nederland"))), "Amsterdam")
    val adq = SingleAnswer("Rotterdam ")
    val adq2 = SingleAnswer("Amsterdam	 ")


    dq.isCorrect(adq) must_== Failure("wrongAnswer")
    dq.isCorrect(adq2) must_== Success(dq)
    dq.asHtml must_== """<div class="label">Wat is de hoofdstad van</div><div class="statement-container"><ul class="list-inline"><li><div class="statement">Nederland</div></li></ul></div><div class="answer"><input type="text" class="form-control" id="answer" placeholder="Amsterdam"></div>"""

    val tree = DICTI(uuid, QuestionBody("Geef de Nederlandse vertaling voor", Some(List("tree"))), "boom")
    val treef = SingleAnswer("arbre")
    val treet = SingleAnswer(" boom ")

    tree.isCorrect(treef) must_== Failure("wrongAnswer")
    tree.isCorrect(treet) must_== Success(tree)
  }

  "DICTQGroup should be converterd to stream of items" in {

    
    ditems must beAnInstanceOf[Stream[com.faqtfinding.pool.Item]]
    ditems must have size(7)

    ditems2 must beAnInstanceOf[Stream[com.faqtfinding.pool.Item]]
    ditems2 must have size(7)
    // items must_== Stream(
    // val i1 = DICTI(Question.uuid,QuestionBody("Wat is de hoofdstad van", Some(List("Nederland"))), "Amsterdam"),
    //   DICTI(Question.uuid,QuestionBody("Wat is de hoofdstad van", Some(List("Belgie"))), "Brussel"),
    //   DICTI(Question.uuid,QuestionBody("Wat is de hoofdstad van", Some(List("Duitsland"))), "Berlijn"),
    //   DICTI(Question.uuid,QuestionBody("Wat is de hoofdstad van", Some(List("Luxemburg"))), "Luxemburg"),
    //   DICTI(Question.uuid,QuestionBody("Wat is de hoofdstad van", Some(List("Frankrijk"))), "Parijs"),
    //   DICTI(Question.uuid,QuestionBody("Wat is de hoofdstad van", Some(List("Spanje"))), "Madrid"),
    //   DICTI(Question.uuid,QuestionBody("Wat is de hoofdstad van", Some(List("Portugal"))), "Lissabon"))

  //   items2 must_== Stream(
  //     DICTI(Question.uuid,QuestionBody("Van welk land is dit de hoofdstad", Some(List("Amsterdam"))), "Nederland"),
  //     DICTI(Question.uuid,QuestionBody("Van welk land is dit de hoofdstad", Some(List("Brussel"))), "Belgie"),
  //     DICTI(Question.uuid,QuestionBody("Van welk land is dit de hoofdstad", Some(List("Berlijn"))), "Duitsland"),
  //     DICTI(Question.uuid,QuestionBody("Van welk land is dit de hoofdstad", Some(List("Luxemburg"))), "Luxemburg"),
  //     DICTI(Question.uuid,QuestionBody("Van welk land is dit de hoofdstad", Some(List("Parijs"))), "Frankrijk"),
  //     DICTI(Question.uuid,QuestionBody("Van welk land is dit de hoofdstad", Some(List("Madrid"))), "Spanje"),
  //     DICTI(Question.uuid,QuestionBody("Van welk land is dit de hoofdstad", Some(List("Lissabon"))), "Portugal"))

  // case class Iteration(current:Zipper[ItemResult], lefts:Stream[ItemResult],rights: Stream[ItemResult]) {

  }

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

  "Multiple Choice Items should work" in {

    "ordered form" in {
      //ordered => the first answers are evaluated until the minimum #answers is reached
      val mcno = MCOI(uuid,QuestionBody("What is the alphabet like"), List("a", "b", "c", "d"), 3)
      val mcno4 = MCOI(uuid,QuestionBody("What is the alphabet like"), List("a", "b", "c", "d"), 4)
      mcno.isCorrect(MultipleAnswers(List("a", "b", "c", "k"))) must_== Success(mcno)
      mcno4.isCorrect(MultipleAnswers(List("a", "b", "c", "k"))) must_== Failure("wrongAnswer")
      mcno.isCorrect(MultipleAnswers(List("e", "b", "c", "d"))) must_== Failure("wrongAnswer")
      mcno.isCorrect(MultipleAnswers(List("e", "b", "d", "a"))) must_== Failure("wrongAnswer~wrongAnswer")
      mcno.isCorrect(MultipleAnswers(List("e", "a", "b", "d"))) must_== Failure("wrongAnswer~wrongAnswer~wrongAnswer") //Should be NEL
      mcno.isCorrect(MultipleAnswers(List("a", "b", "c"))) must_== Success(mcno)
      mcno4.isCorrect(MultipleAnswers(List("a", "b", "c"))) must_== Failure("tooFewAnswers")
      mcno.isCorrect(MultipleAnswers(List("a", "b", "c", "d"))) must_== Success(mcno)
      mcno.asHtml must_== """<div class="label">What is the alphabet like</div><div class="alternative-container"><div class="alternative">a</div><div class="alternative">b</div><div class="alternative">c</div><div class="alternative">d</div></div>"""
    }

    "unordered form" in {
      //unordered => all answers are evaluated
      val mcnu = MCUI(uuid,QuestionBody("What is the alphabet like"), Map(("a", true), ("b", true), ("c", true), ("5", false), ("d", true)), 3)
      val mcnu4 = MCUI(uuid,QuestionBody("What is the alphabet like"), Map(("a", true), ("b", true), ("c", true), ("d", true)), 4)
      mcnu.isCorrect(MultipleAnswers(List("a", "b", "c", "k"))) must_== Failure("answerNotAvailable")
      mcnu.isCorrect(MultipleAnswers(List("a", "b", "c", "5"))) must_== Failure("wrongAnswer")
      mcnu.isCorrect(MultipleAnswers(List("a", "b", "c", "d"))) must_== Success(mcnu)
      mcnu.isCorrect(MultipleAnswers(List("b", "c", "a", "d"))) must_== Success(mcnu)
      mcnu.isCorrect(MultipleAnswers(List("b", "c", "a"))) must_== Success(mcnu)
      mcnu.isCorrect(MultipleAnswers(List("b", "c"))) must_== Failure("tooFewAnswers")

      mcnu4.isCorrect(MultipleAnswers(List("a", "b", "c", "k"))) must_== Failure("answerNotAvailable")
      mcnu4.isCorrect(MultipleAnswers(List("b", "c", "a"))) must_== Failure("tooFewAnswers")
      mcnu4.isCorrect(MultipleAnswers(List("a", "b", "c", "d"))) must_== Success(mcnu4)
      mcnu4.isCorrect(MultipleAnswers(List("b", "c", "a", "d"))) must_== Success(mcnu4)
      mcnu4.isCorrect(MultipleAnswers(List("b", "c", "a", "d", "e"))) must_== Failure("answerNotAvailable")
      mcnu4.isCorrect(MultipleAnswers(List("b", "c", "e", "d", "a"))) must_== Failure("answerNotAvailable")
      mcnu4.asHtml must_== """<div class="label">What is the alphabet like</div><div class="alternative-container"><div class="alternative">a</div><div class="correct">true</div><div class="alternative">b</div><div class="correct">true</div><div class="alternative">c</div><div class="correct">true</div><div class="alternative">d</div><div class="correct">true</div></div>"""
    }

    "and list of alternatives" in {
      // val cijferR = MCUI(Question.uuid,QuestionBody("Welk cijfer vult deze reeks aan", Some(List("1", "4", "9", "16"))), Map(("20", false), ("25", true), ("30", false), ("9", false)), 1)
      val cijferRq = MCUQ(uuid, QuestionBody("Welk cijfer vult deze reeks aan", Some(List("1", "4", "9", "16"))), Map(("20", false), ("25", true), ("30", false), ("9", false)), 1)
      val cijferR = cijferRq.item

      cijferR.isCorrect(MultipleAnswers(List(""))) must_== Failure("answerNotAvailable")
      cijferR.isCorrect(MultipleAnswers(List("20"))) must_== Failure("wrongAnswer")
      cijferR.isCorrect(MultipleAnswers(List("30"))) must_== Failure("wrongAnswer")
      cijferR.isCorrect(MultipleAnswers(List("9"))) must_== Failure("wrongAnswer")
      cijferR.isCorrect(MultipleAnswers(List("12"))) must_== Failure("answerNotAvailable")
      cijferR.isCorrect(MultipleAnswers(List("25"))) must_== Success(cijferR)
      cijferRq.asHtml must_== """<div class="label">Welk cijfer vult deze reeks aan</div><div class="statement-container"><ul class="list-inline"><li><div class="statement">1</div></li><li><div class="statement">4</div></li><li><div class="statement">9</div></li><li><div class="statement">16</div></li></ul></div><div class="alternative-container"><div class="alternative">20</div><div class="correct">false</div><div class="alternative">25</div><div class="correct">true</div><div class="alternative">30</div><div class="correct">false</div><div class="alternative">9</div><div class="correct">false</div></div>"""
      cijferR.asHtml  must_== cijferRq.asHtml
    }
  }
}