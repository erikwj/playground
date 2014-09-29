package pool

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

  val mc1 = MCUQ(QuestionBody("WWII started in 1914"), Map(("Yes", true), ("No", false)), 1)
  val mc1_i = mc1.item

  val q1 = DICTQ("Nederland", "Amsterdam")
  val q2 = DICTQ("Belgie", "Brussel")
  val q3 = DICTQ("Duitsland", "Berlijn")
  val q4 = DICTQ("Luxemburg", "Luxemburg")
  val q5 = DICTQ("Frankrijk", "Parijs")
  val q6 = DICTQ("Spanje", "Madrid")
  val q7 = DICTQ("Portugal", "Lissabon")

  "TrueFalseQuestion => MultipleChoiceQuestion can be answered" in {
    mc1_i.isCorrect(MultipleAnswers(List("Yes"))) must_== Success(mc1_i.id)
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
    val dq = DICTI(Question.uuid,QuestionBody("Wat is de hoofdstad van", Some(List("Nederland"))), "Amsterdam")
    val adq = SingleAnswer("Rotterdam ")
    val adq2 = SingleAnswer("Amsterdam	 ")


    dq.isCorrect(adq) must_== Failure("wrongAnswer")
    dq.isCorrect(adq2) must_== Success(dq.id)
    dq.asHtml must_== """<div class="label">Wat is de hoofdstad van</div><div class="statement-container"><ul class="list-inline"><li><div class="statement">Nederland</div></li></ul></div><div class="answer"><input type="text" class="form-control" id="answer" placeholder="Amsterdam"></div>"""

    val tree = DICTI(Question.uuid,QuestionBody("Geef de Nederlandse vertaling voor", Some(List("tree"))), "boom")
    val treef = SingleAnswer("arbre")
    val treet = SingleAnswer(" boom ")

    tree.isCorrect(treef) must_== Failure("wrongAnswer")
    tree.isCorrect(treet) must_== Success(tree.id)
  }

  "DICTQGroup should be converterd to stream of items" in {

    val qg = DICTQGroup("Wat is de hoofdstad van", Some("Van welk land is dit de hoofdstad"), Stream(q1, q2, q3, q4, q5, q6, q7))
    val items = qg.items(false)
    val items2 = qg.items(true)

    items must beAnInstanceOf[Stream[com.faqtfinding.pool.Item]]
    items must have size(7)

    items2 must beAnInstanceOf[Stream[com.faqtfinding.pool.Item]]
    items2 must have size(7)
    // items must_== Stream(
    //   DICTI(Question.uuid,QuestionBody("Wat is de hoofdstad van", Some(List("Nederland"))), "Amsterdam"),
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
  }

  "Multiple Choice Items should work" in {

    "ordered form" in {
      //ordered => the first answers are evaluated until the minimum #answers is reached
      val mcno = MCOI(Question.uuid,QuestionBody("What is the alphabet like"), List("a", "b", "c", "d"), 3)
      val mcno4 = MCOI(Question.uuid,QuestionBody("What is the alphabet like"), List("a", "b", "c", "d"), 4)
      mcno.isCorrect(MultipleAnswers(List("a", "b", "c", "k"))) must_== Success(mcno.id + mcno.id + mcno.id)
      mcno4.isCorrect(MultipleAnswers(List("a", "b", "c", "k"))) must_== Failure("wrongAnswer")
      mcno.isCorrect(MultipleAnswers(List("e", "b", "c", "d"))) must_== Failure("wrongAnswer")
      mcno.isCorrect(MultipleAnswers(List("e", "b", "d", "a"))) must_== Failure("wrongAnswerwrongAnswer")
      mcno.isCorrect(MultipleAnswers(List("e", "a", "b", "d"))) must_== Failure("wrongAnswerwrongAnswerwrongAnswer") //Should be NEL
      mcno.isCorrect(MultipleAnswers(List("a", "b", "c"))) must_== Success(mcno.id + mcno.id + mcno.id)
      mcno4.isCorrect(MultipleAnswers(List("a", "b", "c"))) must_== Failure("tooFewAnswers")
      mcno.isCorrect(MultipleAnswers(List("a", "b", "c", "d"))) must_== Success(mcno.id + mcno.id + mcno.id)
      mcno.asHtml must_== """<div class="label">What is the alphabet like</div><div class="alternative-container"><div class="alternative">a</div><div class="alternative">b</div><div class="alternative">c</div><div class="alternative">d</div></div>"""
    }

    "unordered form" in {
      //unordered => all answers are evaluated
      val mcnu = MCUI(Question.uuid,QuestionBody("What is the alphabet like"), Map(("a", true), ("b", true), ("c", true), ("5", false), ("d", true)), 3)
      val mcnu4 = MCUI(Question.uuid,QuestionBody("What is the alphabet like"), Map(("a", true), ("b", true), ("c", true), ("d", true)), 4)
      mcnu.isCorrect(MultipleAnswers(List("a", "b", "c", "k"))) must_== Failure("answerNotAvailable")
      mcnu.isCorrect(MultipleAnswers(List("a", "b", "c", "5"))) must_== Failure("wrongAnswer")
      mcnu.isCorrect(MultipleAnswers(List("a", "b", "c", "d"))) must_== Success(mcnu.id + mcnu.id + mcnu.id+ mcnu.id)
      mcnu.isCorrect(MultipleAnswers(List("b", "c", "a", "d"))) must_== Success(mcnu.id + mcnu.id + mcnu.id+ mcnu.id)
      mcnu.isCorrect(MultipleAnswers(List("b", "c", "a"))) must_== Success(mcnu.id + mcnu.id + mcnu.id)
      mcnu.isCorrect(MultipleAnswers(List("b", "c"))) must_== Failure("tooFewAnswers")

      mcnu4.isCorrect(MultipleAnswers(List("a", "b", "c", "k"))) must_== Failure("answerNotAvailable")
      mcnu4.isCorrect(MultipleAnswers(List("b", "c", "a"))) must_== Failure("tooFewAnswers")
      mcnu4.isCorrect(MultipleAnswers(List("a", "b", "c", "d"))) must_== Success(mcnu4.id + mcnu4.id + mcnu4.id+ mcnu4.id)
      mcnu4.isCorrect(MultipleAnswers(List("b", "c", "a", "d"))) must_== Success(mcnu4.id + mcnu4.id + mcnu4.id+ mcnu4.id)
      mcnu4.isCorrect(MultipleAnswers(List("b", "c", "a", "d", "e"))) must_== Failure("answerNotAvailable")
      mcnu4.isCorrect(MultipleAnswers(List("b", "c", "e", "d", "a"))) must_== Failure("answerNotAvailable")
      mcnu4.asHtml must_== """<div class="label">What is the alphabet like</div><div class="alternative-container"><div class="alternative">a</div><div class="correct">true</div><div class="alternative">b</div><div class="correct">true</div><div class="alternative">c</div><div class="correct">true</div><div class="alternative">d</div><div class="correct">true</div></div>"""
    }

    "and list of alternatives" in {
      // val cijferR = MCUI(Question.uuid,QuestionBody("Welk cijfer vult deze reeks aan", Some(List("1", "4", "9", "16"))), Map(("20", false), ("25", true), ("30", false), ("9", false)), 1)
      val cijferRq = MCUQ(QuestionBody("Welk cijfer vult deze reeks aan", Some(List("1", "4", "9", "16"))), Map(("20", false), ("25", true), ("30", false), ("9", false)), 1)
      val cijferR = cijferRq.item

      cijferR.isCorrect(MultipleAnswers(List(""))) must_== Failure("answerNotAvailable")
      cijferR.isCorrect(MultipleAnswers(List("20"))) must_== Failure("wrongAnswer")
      cijferR.isCorrect(MultipleAnswers(List("30"))) must_== Failure("wrongAnswer")
      cijferR.isCorrect(MultipleAnswers(List("9"))) must_== Failure("wrongAnswer")
      cijferR.isCorrect(MultipleAnswers(List("12"))) must_== Failure("answerNotAvailable")
      cijferR.isCorrect(MultipleAnswers(List("25"))) must_== Success(cijferR.id)
      cijferRq.asHtml must_== """<div class="label">Welk cijfer vult deze reeks aan</div><div class="statement-container"><ul class="list-inline"><li><div class="statement">1</div></li><li><div class="statement">4</div></li><li><div class="statement">9</div></li><li><div class="statement">16</div></li></ul></div><div class="alternative-container"><div class="alternative">20</div><div class="correct">false</div><div class="alternative">25</div><div class="correct">true</div><div class="alternative">30</div><div class="correct">false</div><div class="alternative">9</div><div class="correct">false</div></div>"""
      cijferR.asHtml  must_== cijferRq.asHtml
    }
  }
}