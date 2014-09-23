package pool

import org.specs2.mutable.Specification
import pool.HTTree.Answer

object QuestionsSpec extends Specification {
  import pool.Question._
  import pool.HTTree._

  val tfq = TFQ(ItemBody("WWII started in 1914"), false)
  val a1 = Answer(tfq.body.label, true)
  val a2 = Answer(tfq.body.label, false)
  val a3 = Answer("WWII started in 1944", true)
  val a4 = Answer("WWII started in 1944", false)
  val a5 = Answer("  WWII started in 1914 ", false)

  //TODO tab deletion needs attention
  val a6 = Answer("WWII  		 started in 1914 ", false)

  val q1 = DICTQ("Nederland", "Amsterdam")
  val q2 = DICTQ("Belgie", "Brussel")
  val q3 = DICTQ("Duitsland", "Berlijn")
  val q4 = DICTQ("Luxemburg", "Luxemburg")
  val q5 = DICTQ("Frankrijk", "Parijs")
  val q6 = DICTQ("Spanje", "Madrid")
  val q7 = DICTQ("Portugal", "Lissabon")

  "TFQ can be answered" in {
    tfq.isCorrect(a1) must_== false
    tfq.isCorrect(a2) must_== true
    tfq.isCorrect(a3) must_== false
    tfq.isCorrect(a4) must_== false
    tfq.isCorrect(a5) must_== true
    tfq.isCorrect(a6) must_== true
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
}