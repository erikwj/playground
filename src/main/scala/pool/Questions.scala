package pool

import pool.HTTree.AnswerResult
import pool.Question.DICTQ
import scalaz._
import Scalaz._

sealed trait Question

object Question {

  case class DICTQ(body: String, answer: String) extends Question {

    def item(instruction: String): DICTI = DICTI(ItemBody(instruction, Some(List(body))), answer)
    def reverseItem(reverseInstruction: String): DICTI = DICTI(ItemBody(reverseInstruction, Some(List(answer))), body)

  }

  case class DICTQGroup(instruction: String, reverseInstruction: Option[String], questions: Stream[DICTQ]) {
    def items(reverse: Boolean): Stream[Item] =
      if (reverse) reverseInstruction match {
        case Some(ri) => questions map { _.reverseItem(ri) }
        case _ => Stream.Empty
      }
      else questions map { _.item(instruction) }
  }

  case class MCOQ(body: ItemBody, alternatives: List[String], minimum: Int) extends Question {
    def item: Item = MCOI(body, alternatives, minimum)
  }

  case class MCUQ(body: ItemBody, alternatives: Map[String, Boolean], minimum: Int) extends Question {
    def item: Item = MCUI(body, alternatives, minimum)
  }
}

case class ItemBody(label: String, statements: Option[List[String]] = None)

sealed trait Item {
  def body: ItemBody
  def label: String = body.label
  def clean(s: String): String = s.replaceAll("\\s+", " ").trim

  def isCorrect(a: Answers): Validation[String, String]
}

case class DICTI(body: ItemBody, answer: String) extends Item {
  val strict: Boolean = true

  def isCorrect(a: Answers) =
    if (strict) {
      if (clean(answer) == clean(a.input.head)) Success(clean(a.input.head))
      else Failure("""wrongAnswer""")
    } else {
      if (clean(a.input.head).toLowerCase() == answer.toLowerCase()) Success(clean(a.input.head))
      else Failure("""wrongAnswer""")
    }
}

case class MCOI(body: ItemBody, alternatives: List[String], minimum: Int) extends Item {

  private def check(answer: String, correct: String) =
    if (clean(answer) == correct) Success(clean(answer))
    else Failure("""wrongAnswer""")

  //@tailrec
  private def orderedChecker(as: List[String], corrects: List[String], b: Validation[String, String], m: Int): Validation[String, String] =
    if (m == 0) b
    else {
      (as, corrects) match {
        case (h :: t, h2 :: t2) =>
          val acc = b +++ check(h, h2)
          if (acc.isSuccess) orderedChecker(t, t2, acc, m - 1)
          else Failure("""wrongAnswer""")
        case (Nil, h2 :: t2) => Failure("tooFewAnswers")
        case _ => b
      }
    }

  def isCorrect(a: Answers) =
    if (a.input.length < minimum) Failure("tooFewAnswers")
    else { orderedChecker(a.input, alternatives, Success(""), minimum) }
}

case class MCUI(body: ItemBody, alternatives: Map[String, Boolean], minimum: Int) extends Item {

  def evaluate(a: String): Validation[String, String] = {
    val ca = clean(a)
    val b = alternatives.get(ca)
    b match {
      case Some(bb) => if (bb) Success(ca) else Failure("""wrongAnswer""")
      case _ => Failure("answerNotAvailable")
    }
  }

  def isCorrect(a: Answers) =
    if (a.input.length < minimum) Failure("tooFewAnswers")
    else (a.input map { evaluate(_) }).reduce(_ +++ _)

}

sealed trait Answers {
  def input: List[String]
}

object Answers {
  case class SingleAnswer(a: String) extends Answers {
    def input = List(a)
  }

  case class MultipleAnswers(input: List[String]) extends Answers {
  }
}
