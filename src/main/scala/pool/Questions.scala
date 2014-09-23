package pool

import pool.HTTree.Answer
import pool.Question.DICTQ

sealed trait Question {
  //  def body: QuestionBody
  //  def label: String = body.label

  def clean(s: String): String = s.replaceAll("\\s+", " ").trim
  //  def alternatives: Map[String, Boolean]
  //  def isCorrect(a: Answer[String]): Boolean //Either[String,Boolean]
}

object Question {

  case class TFQ(body: ItemBody, correct: Boolean) extends Question {
    def alternatives: Map[String, Boolean] = Map((body.label, correct))
    def isCorrect(a: Answer[String]) = {
      val b = alternatives.get(body.label)
      val tf = b match {
        case Some(v) => v
        case _ => false
      }
      clean(a.q) == body.label && a.r == tf
    }
  }

  //    object TFQ {}

  //  case class OLQ(body:QuestionBody,alternatives:Map[String,Boolean]) extends Question
  //  case class ULQ(body:QuestionBody,alternatives:Map[String,Boolean]) extends Question

  case class DICTQ(body: String, answer: String) extends Question {

  }
  //  case class MC1Q(body:QuestionBody,alternatives:Map[String,Boolean]) extends Question
  //  case class MCNQ(body:QuestionBody,alternatives:Map[String,Boolean]) extends Question
}

case class DICTQGroup(instruction: String, reverseInstruction: Option[String], questions: Stream[DICTQ]) {
  def items(reverse: Boolean) =
    if (reverse) reverseInstruction match {
      case Some(ri) => questions map { q => DICTI(ItemBody(ri, Some(List(q.answer))), q.body) }
      case _ => sys.error("Sorry no instruction for this question")
    }
    else questions map { q => DICTI(ItemBody(instruction, Some(List(q.body))), q.answer) }
}

object QuestionGroup {

}

case class ItemBody(label: String, statements: Option[List[String]] = None)

sealed trait Item {
  def body: ItemBody
  def label: String = body.label

  def clean(s: String): String = s.replaceAll("\\s+", " ").trim
}

case class DICTI(body: ItemBody, answer: String) extends Item {
  def isCorrect(s: String, strict: Boolean = true) =
    if (strict) clean(s) == answer
    else clean(s).toLowerCase() == answer.toLowerCase()
}

