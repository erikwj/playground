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

  //  case class OLQ(body:QuestionBody,alternatives:Map[String,Boolean]) extends Question
  //  case class ULQ(body:QuestionBody,alternatives:Map[String,Boolean]) extends Question

  case class DICTQ(body: String, answer: String) extends Question {

    def item(instruction: String) = DICTI(ItemBody(instruction, Some(List(body))), answer)
    def reverseItem(reverseInstruction: String) = DICTI(ItemBody(reverseInstruction, Some(List(answer))), body)

  }
  case class MC1Q(body: ItemBody, alternatives: Map[String, Boolean]) extends Question {
    def item = MCUI(body, alternatives,1)
  }

  case class MCNOQ(body: ItemBody, alternatives: List[(String, Boolean)], minimum: Int) extends Question {
    def item = MCOI(body, alternatives, minimum)
  }

  case class MCNUQ(body: ItemBody, alternatives: Map[String, Boolean], minimum: Int) extends Question {
    def item = MCUI(body, alternatives, minimum)
  }
  //  case class MCNQ(body:QuestionBody,alternatives:Map[String,Boolean]) extends Question
}

case class DICTQGroup(instruction: String, reverseInstruction: Option[String], questions: Stream[DICTQ]) {
  def items(reverse: Boolean) =
    if (reverse) reverseInstruction match {
      case Some(ri) => questions map { _.reverseItem(ri) }
      case _ => sys.error("Sorry no instruction for this question")
    }
    else questions map { _.item(instruction) }
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

case class MCOI(body: ItemBody, alternatives: List[(String, Boolean)], minimum: Int) extends Item {

  def check(answer: String, pair: (String, Boolean)) = (clean(answer) == pair._1) && pair._2

  def orderedChecker(as: List[String], pairs: List[(String, Boolean)], b: Boolean, m: Int): Boolean =
    if (m == 0) b
    else {
      (as, pairs) match {
        case (h :: t, h2 :: t2) =>
          if (check(h, h2) && b) orderedChecker(t, t2, true, m-1)
          else false
        case (Nil, h2 :: t2) => false
        case (_, Nil) => b
        case _ => b
      }
    }
  
  def isCorrect(as: List[String]) = 
    if (as.length < minimum) false
    else { orderedChecker(as, alternatives, true, minimum) }
}

case class MCUI(body: ItemBody, alternatives: Map[String, Boolean], minimum: Int) extends Item {

  def isCorrect(as: List[String]) = if (as.length < minimum) false
    else {
    val bs = as map { a =>
      val b = alternatives.get(clean(a))
      b match {
        case Some(bb) => bb
        case _ => false
      }
    }
    bs.foldLeft(true)(_ && _)
  }

}

