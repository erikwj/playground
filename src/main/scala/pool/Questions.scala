package pool

import pool.HTTree.Answer
import pool.Question.DICTQ
import scalaz._
import Scalaz._


sealed trait Question {
  //  def body: QuestionBody
  //  def label: String = body.label

  def clean(s: String): String = s.replaceAll("\\s+", " ").trim
  //  def alternatives: Map[String, Boolean]
  //  def isCorrect(a: Answer[String]): Boolean //Either[String,Boolean]
}

object Question {

  //  case class OLQ(body:QuestionBody,alternatives:Map[String,Boolean]) extends Question
  //  case class ULQ(body:QuestionBody,alternatives:Map[String,Boolean]) extends Question

  case class DICTQ(body: String, answer: String) extends Question {

    def item(instruction: String) = DICTI(ItemBody(instruction, Some(List(body))), answer)
    def reverseItem(reverseInstruction: String) = DICTI(ItemBody(reverseInstruction, Some(List(answer))), body)

  }

  case class DICTQGroup(instruction: String, reverseInstruction: Option[String], questions: Stream[DICTQ]) {
    def items(reverse: Boolean) =
      if (reverse) reverseInstruction match {
        case Some(ri) => questions map { _.reverseItem(ri) }
        case _ => sys.error("Sorry no instruction for this question")
      }
      else questions map { _.item(instruction) }
  }

  case class MCOQ(body: ItemBody, alternatives: List[String], minimum: Int) extends Question {
    def item = MCOI(body, alternatives, minimum)
  }

  case class MCUQ(body: ItemBody, alternatives: Map[String, Boolean], minimum: Int) extends Question {
    def item = MCUI(body, alternatives, minimum)
  }
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

case class MCOI(body: ItemBody, alternatives: List[String], minimum: Int) extends Item {

  private def check(answer: String, correct: String) = clean(answer) == correct

  //@tailrec
  private def orderedChecker(as: List[String], corrects: List[String], b: Boolean, m: Int): Boolean =
    if (m == 0) b
    else {
      (as, corrects) match {
        case (h :: t, h2 :: t2) =>
          if (check(h, h2) && b) orderedChecker(t, t2, true, m - 1)
          else false
        case (Nil, h2 :: t2) => false
        case _ => b
      }
    }

  def isCorrect(as: List[String]) =
    if (as.length < minimum) false
    else { orderedChecker(as, alternatives, true, minimum) }
}

case class MCUI(body: ItemBody, alternatives: Map[String, Boolean], minimum: Int) extends Item {

  def evaluate(a:String):Validation[String,String] = { 
      val ca = clean(a)
      val b = alternatives.get(ca)
      b match {
        case Some(bb) => if(bb) Success(ca) else Failure("""wrongAnswer""")
        case _ => Failure("answerNotAvailable")
      }
    }
  
  
  def isCorrect(as: List[String]) = 
    if (as.length < minimum) Failure("tooFewAnswers")
    else (as map {evaluate(_)}).reduce(_ +++ _)

}

