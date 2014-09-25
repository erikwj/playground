package pool

import org.specs2.mutable.Specification
import pool.HTTree.AnswerResult
import scalaz._
import Scalaz._

object QuestionsSpec extends Specification {
  import pool.Question._
  import pool.Answers._
  import pool.HTTree._

  val mc1 = MCUQ(ItemBody("WWII started in 1914"), Map(("Yes", true), ("No", false)), 1)
  val mc1_i = mc1.item

  val q1 = DICTQ("Nederland", "Amsterdam")
  val q2 = DICTQ("Belgie", "Brussel")
  val q3 = DICTQ("Duitsland", "Berlijn")
  val q4 = DICTQ("Luxemburg", "Luxemburg")
  val q5 = DICTQ("Frankrijk", "Parijs")
  val q6 = DICTQ("Spanje", "Madrid")
  val q7 = DICTQ("Portugal", "Lissabon")

  "TrueFalseQuestion => MultipleChoiceQuestion can be answered" in {
    mc1_i.isCorrect(MultipleAnswers(List("Yes"))) must_== Success("Yes")
    mc1_i.isCorrect(MultipleAnswers(List("No"))) must_== Failure("wrongAnswer")
    mc1_i.isCorrect(MultipleAnswers(List("Yes", "No"))) must_== Failure("wrongAnswer")
    mc1_i.isCorrect(MultipleAnswers(List("No", "Yes"))) must_== Failure("wrongAnswer")
  }

  "DICTI can be answered" in {
    val dq = DICTI(ItemBody("Wat is de hoofdstad van", Some(List("Nederland"))), "Amsterdam")
    val adq = SingleAnswer("Rotterdam ")
    val adq2 = SingleAnswer("Amsterdam	 ")

    dq.isCorrect(adq) must_== Failure("wrongAnswer")
    dq.isCorrect(adq2) must_== Success("Amsterdam")

    val tree = DICTI(ItemBody("Geef de Nederlandse vertaling voor", Some(List("tree"))), "boom")
    val treef = SingleAnswer("arbre")
    val treet = SingleAnswer(" boom ")

    tree.isCorrect(treef) must_== Failure("wrongAnswer")
    tree.isCorrect(treet) must_== Success("boom")
  }

  "DICTQGroup should be converterd to stream of items" in {

    val qg = DICTQGroup("Wat is de hoofdstad van", Some("Van welk land is dit de hoofdstad"), Stream(q1, q2, q3, q4, q5, q6, q7))
    val items = qg.items(false)
    val items2 = qg.items(true)

    items must_== Stream(
      DICTI(ItemBody("Wat is de hoofdstad van", Some(List("Nederland"))), "Amsterdam"),
      DICTI(ItemBody("Wat is de hoofdstad van", Some(List("Belgie"))), "Brussel"),
      DICTI(ItemBody("Wat is de hoofdstad van", Some(List("Duitsland"))), "Berlijn"),
      DICTI(ItemBody("Wat is de hoofdstad van", Some(List("Luxemburg"))), "Luxemburg"),
      DICTI(ItemBody("Wat is de hoofdstad van", Some(List("Frankrijk"))), "Parijs"),
      DICTI(ItemBody("Wat is de hoofdstad van", Some(List("Spanje"))), "Madrid"),
      DICTI(ItemBody("Wat is de hoofdstad van", Some(List("Portugal"))), "Lissabon"))

    items2 must_== Stream(
      DICTI(ItemBody("Van welk land is dit de hoofdstad", Some(List("Amsterdam"))), "Nederland"),
      DICTI(ItemBody("Van welk land is dit de hoofdstad", Some(List("Brussel"))), "Belgie"),
      DICTI(ItemBody("Van welk land is dit de hoofdstad", Some(List("Berlijn"))), "Duitsland"),
      DICTI(ItemBody("Van welk land is dit de hoofdstad", Some(List("Luxemburg"))), "Luxemburg"),
      DICTI(ItemBody("Van welk land is dit de hoofdstad", Some(List("Parijs"))), "Frankrijk"),
      DICTI(ItemBody("Van welk land is dit de hoofdstad", Some(List("Madrid"))), "Spanje"),
      DICTI(ItemBody("Van welk land is dit de hoofdstad", Some(List("Lissabon"))), "Portugal"))
  }

  "MCNQ should work" in {
    //ordered => the first answers are evaluated until the minimum #answers is reached
    val mcno = MCOI(ItemBody("What is the alphabet like"), List("a", "b","c","d"), 3)
    val mcno4 = MCOI(ItemBody("What is the alphabet like"), List("a", "b","c","d"), 4)
    mcno.isCorrect(MultipleAnswers(List("a", "b", "c", "k"))) must_== Success("abc")
    mcno4.isCorrect(MultipleAnswers(List("a", "b", "c", "k"))) must_== Failure("wrongAnswer")
    mcno.isCorrect(MultipleAnswers(List("e", "a", "b", "c", "d"))) must_== Failure("wrongAnswer")
    mcno.isCorrect(MultipleAnswers(List("a", "b", "c"))) must_== Success("abc")
    mcno4.isCorrect(MultipleAnswers(List("a", "b", "c"))) must_== Failure("tooFewAnswers")
    mcno.isCorrect(MultipleAnswers(List("a", "b", "c", "d"))) must_== Success("abc")

    //unordered => all answers are evaluated
    val mcnu = MCUI(ItemBody("What is the alphabet like"), Map(("a", true), ("b", true), ("c", true),("5",false), ("d", true)), 3)
    val mcnu4 = MCUI(ItemBody("What is the alphabet like"), Map(("a", true), ("b", true), ("c", true), ("d", true)), 4)
    mcnu.isCorrect(MultipleAnswers(List("a", "b", "c", "k"))) must_== Failure("answerNotAvailable")
    mcnu.isCorrect(MultipleAnswers(List("a", "b", "c", "d"))) must_== Success("abcd")
    mcnu.isCorrect(MultipleAnswers(List("b", "c", "a", "d"))) must_== Success("bcad")
    mcnu.isCorrect(MultipleAnswers(List("b", "c", "a"))) must_== Success("bca")
    mcnu.isCorrect(MultipleAnswers(List("b", "c"))) must_== Failure("tooFewAnswers")

    mcnu4.isCorrect(MultipleAnswers(List("a", "b", "c", "k"))) must_== Failure("answerNotAvailable")
    mcnu4.isCorrect(MultipleAnswers(List("b", "c", "a"))) must_== Failure("tooFewAnswers")
    mcnu4.isCorrect(MultipleAnswers(List("a", "b", "c", "d"))) must_== Success("abcd")
    mcnu4.isCorrect(MultipleAnswers(List("b", "c", "a", "d"))) must_== Success("bcad")
    mcnu4.isCorrect(MultipleAnswers(List("b", "c", "a", "d", "e"))) must_== Failure("answerNotAvailable")
    mcnu4.isCorrect(MultipleAnswers(List("b", "c", "e", "d", "a"))) must_== Failure("answerNotAvailable")

    val cijferR = MCUI(ItemBody("Welk cijfer vult deze reeks aan", Some(List("1", "4", "9", "16"))), Map(("20", false), ("25", true), ("30", false), ("9", false)), 1)
    cijferR.isCorrect(MultipleAnswers(List(""))) must_== Failure("answerNotAvailable")
    cijferR.isCorrect(MultipleAnswers(List("20"))) must_== Failure("wrongAnswer")
    cijferR.isCorrect(MultipleAnswers(List("30"))) must_== Failure("wrongAnswer")
    cijferR.isCorrect(MultipleAnswers(List("9"))) must_== Failure("wrongAnswer")
    cijferR.isCorrect(MultipleAnswers(List("12"))) must_== Failure("answerNotAvailable")
    cijferR.isCorrect(MultipleAnswers(List("25"))) must_== Success("25")
    
  }
}