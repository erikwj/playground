package pool

import pool.HTTree.AnswerResult
import pool.Question.DICTQ
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads

import scalaz._
import Scalaz._

sealed trait Question

/**
 * Question contain information. These are unique and can be converted to Items which aren't unique
 */
object Question {

  import Item._
  import QuestionBody._
  
    case class QuestionBody(label: String, statements: Option[List[String]] = None) {
    def asHtml: String = ???

  }

  object QuestionBody {
    //json reader

    implicit val questionBodyReader: Reads[QuestionBody] = {
      (
        (__ \ "label").read[String] and
        (__ \ "statements").readNullable[List[String]])(QuestionBody.apply _)
    }
    
    

  }
  

  case class DICTQ(body: String, answer: String) extends Question {

    def item(instruction: String): Item = DICTI(QuestionBody(instruction, Some(List(body))), answer)
    def reverseItem(reverseInstruction: String): Item = DICTI(QuestionBody(reverseInstruction, Some(List(answer))), body)
def asHtml:String = ???
  }
  object DICTQ {
    implicit val reader: Reads[DICTQ] = {
      (
        (__ \ "body").read[String] and
        (__ \ "answer").read[String])(DICTQ.apply _)
    }
    
      
  
  }

  /**
   *
   * Group contains instruction for list of dictionaire items
   */
  case class DICTQGroup(instruction: String, reverseInstruction: Option[String], questions: Stream[DICTQ]) {
    def items(reverse: Boolean): Stream[Item] =
      if (reverse) reverseInstruction match {
        case Some(ri) => questions map { _.reverseItem(ri) }
        case _ => Stream.Empty
      }
      else questions map { _.item(instruction) }
    
    def asHtml:String = ???
  }

  object DICTQGroup {

  }

  /**
   *
   * Multiple Choice Ordered Question
   *
   */
  case class MCOQ(body: QuestionBody, alternatives: List[String], minimum: Int) extends Question {
    def item: Item = MCOI(body, alternatives, minimum)
    
    def asHtml:String = ???
    
  }

  object MCOQ {
    implicit val reader: Reads[MCOQ] = {
      (
        (__ \ "body").read[QuestionBody](questionBodyReader) and
        (__ \ "alternatives").read[List[String]] and
        (__ \ "minimum").read[Int])(MCOQ.apply _)
    }
    
      
  
  }

  /**
   *
   * Multiple Choice Unordered Question
   *
   */
  case class MCUQ(body: QuestionBody, alternatives: Map[String, Boolean], minimum: Int) extends Question {
    def item: Item = MCUI(body, alternatives, minimum)
    
     def asHtml:String = ???
  }

  object MCUQ {
    implicit val reader: Reads[MCUQ] = {
      (
        (__ \ "body").read[QuestionBody](questionBodyReader) and
        (__ \ "alternatives").read[Map[String, Boolean]] and
        (__ \ "minimum").read[Int])(MCUQ.apply _)
    }
    
   
  }
  
    
  
}

/**
 * Items are derived from questions but are answered and have a score
 */
sealed trait Item {
  import Item._
  import Question.QuestionBody
  def body: QuestionBody
  def label: String = body.label

  def isCorrect(a: Answers): Validation[String, String]
}

object Item {

  import Question.QuestionBody
  def clean(s: String): String = s.replaceAll("\\s+", " ").trim



  /**
   * Dictionnary Item is bidirectional
   */
  case class DICTI(body: QuestionBody, answer: String) extends Item {
    val strict: Boolean = true

    def isCorrect(a: Answers) =
      if (strict) {
        if (clean(answer) == clean(a.input.head)) Success(clean(a.input.head))
        else Failure("""wrongAnswer""")
      } else {
        if (clean(a.input.head).toLowerCase() == answer.toLowerCase()) Success(clean(a.input.head))
        else Failure("""wrongAnswer""")
      }
    
    def asHtml:String = ???
  
  }

  object DICTI {
    
  }

  /**
   * Multiple Choice Ordered Item
   */
  case class MCOI(body: QuestionBody, alternatives: List[String], minimum: Int) extends Item {

    private def answerCompare(answer: String, correct: String) =
      if (clean(answer) == correct) Success(clean(answer))
      else Failure("""wrongAnswer""")

    //@tailrec
    private def evaluate(as: List[String], corrects: List[String]): Validation[String, String] = {
      (as zip corrects)
        .take(minimum)
        .map(p => answerCompare(p._1, p._2))
        .reduce(_ +++ _)
    }

    def isCorrect(a: Answers) =
      if (a.input.length < minimum) Failure("tooFewAnswers")
      else { evaluate(a.input, alternatives) }
    
     def asHtml:String = ???
  }

  object MCOI {
   
  }

  /**
   * Multiple Choice Unordered Item
   */
  case class MCUI(body: QuestionBody, alternatives: Map[String, Boolean], minimum: Int) extends Item {

    private def evaluate(a: String): Validation[String, String] = {
      val ca = clean(a)
      alternatives.get(ca) match {
        case Some(bb) => if (bb) Success(ca) else Failure("""wrongAnswer""")
        case _ => Failure("answerNotAvailable")
      }
    }

    def isCorrect(a: Answers) =
      if (a.input.length < minimum) Failure("tooFewAnswers")
      else (a.input map { evaluate(_) }).reduce(_ +++ _)

    def asHtml:String = ???
    
  }

  object MCUI {
    
  
  }
  
  case class ItemResponse(itemId:String,answers:Answers)
  object ItemResponse {
//        implicit val reader: Reads[ItemResponse] = {
//      (
//        (__ \ "item").read[String] and
//        (__ \ "answers").read[List[String]]
//        )(ItemResponse.apply _)
//    }
  }

}

/**
 * Answers for Item input
 */
sealed trait Answers {
  def input: List[String]
}

object Answers {
  case class SingleAnswer(a: String) extends Answers {
    def input = List(a)
  }

  case class MultipleAnswers(input: List[String]) extends Answers

}
