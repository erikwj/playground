package pool

import org.specs2.mutable.Specification
import pool.HTTree.Answer
import scalaz._
import Scalaz._

object QuestionsSpec extends Specification {
  import pool.Question._
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
    mc1_i.isCorrect(List("Yes")) must_== Success("Yes")
    mc1_i.isCorrect(List("No")) must_== Failure("wrongAnswer")
    mc1_i.isCorrect(List("Yes", "No")) must_== Failure("wrongAnswer")
    mc1_i.isCorrect(List("No", "Yes")) must_== Failure("wrongAnswer")
  }

  "DICTI can be answered" in {
    val dq = DICTI(ItemBody("Wat is de hoofdstad van", Some(List("Nederland"))), "Amsterdam")
    val adq = "Rotterdam "
    val adq2 = "Amsterdam	 "

    dq.isCorrect(adq) must_== false
    dq.isCorrect(adq2) must_== true

    val tree = DICTI(ItemBody("Geef de Nederlandse vertaling voor", Some(List("tree"))), "boom")
    val treef = "arbre"
    val treet = " boom "

    tree.isCorrect(treef) must_== false
    tree.isCorrect(treet) must_== true
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
    mcno.isCorrect(List("a", "b", "c", "k")) must_== true
    mcno4.isCorrect(List("a", "b", "c", "k")) must_== false
    mcno.isCorrect(List("e", "a", "b", "c", "d")) must_== false
    mcno.isCorrect(List("a", "b", "c")) must_== true
    mcno4.isCorrect(List("a", "b", "c")) must_== false
    mcno.isCorrect(List("a", "b", "c", "d")) must_== true

    //unordered => all answers are evaluated
    val mcnu = MCUI(ItemBody("What is the alphabet like"), Map(("a", true), ("b", true), ("c", true),("5",false), ("d", true)), 3)
    val mcnu4 = MCUI(ItemBody("What is the alphabet like"), Map(("a", true), ("b", true), ("c", true), ("d", true)), 4)
    mcnu.isCorrect(List("a", "b", "c", "k")) must_== Failure("answerNotAvailable")
    mcnu.isCorrect(List("a", "b", "c", "d")) must_== Success("abcd")
    mcnu.isCorrect(List("b", "c", "a", "d")) must_== Success("bcad")
    mcnu.isCorrect(List("b", "c", "a")) must_== Success("bca")
    mcnu.isCorrect(List("b", "c")) must_== Failure("tooFewAnswers")

    mcnu4.isCorrect(List("a", "b", "c", "k")) must_== Failure("answerNotAvailable")
    mcnu4.isCorrect(List("b", "c", "a")) must_== Failure("tooFewAnswers")
    mcnu4.isCorrect(List("a", "b", "c", "d")) must_== Success("abcd")
    mcnu4.isCorrect(List("b", "c", "a", "d")) must_== Success("bcad")
    mcnu4.isCorrect(List("b", "c", "a", "d", "e")) must_== Failure("answerNotAvailable")
    mcnu4.isCorrect(List("b", "c", "e", "d", "a")) must_== Failure("answerNotAvailable")

    val cijferR = MCUI(ItemBody("Welk cijfer vult deze reeks aan", Some(List("1", "4", "9", "16"))), Map(("20", false), ("25", true), ("30", false), ("9", false)), 1)
    cijferR.isCorrect(List("")) must_== Failure("answerNotAvailable")
    cijferR.isCorrect(List("20")) must_== Failure("wrongAnswer")
    cijferR.isCorrect(List("30")) must_== Failure("wrongAnswer")
    cijferR.isCorrect(List("9")) must_== Failure("wrongAnswer")
    cijferR.isCorrect(List("12")) must_== Failure("answerNotAvailable")
    cijferR.isCorrect(List("25")) must_== Success("25")
    
  }
}