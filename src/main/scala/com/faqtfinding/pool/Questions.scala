package com.faqtfinding.pool

import play.api.libs.json._
import play.api.libs.json.Reads
import play.api.libs.functional.syntax._
import scalaz._
import scalaz.Scalaz._


sealed trait Question {
  def qid:String
  def asHtml:String
  def item:Item
}


/**
 * Question contain information. These are unique and can be converted to Items which aren't unique
 */
object Question {

  import Item._
  import QuestionBody._

  def uuid = java.util.UUID.randomUUID.toString
  

  //  private val ValidDocRef = """([\w-]{16})""".r

  case class QuestionBody(label: String, statements: Option[List[String]] = None)

  object QuestionBody {
    //json reader

    implicit val questionBodyReader: Reads[QuestionBody] = {
      (
        (__ \ "label").read[String] and
        (__ \ "statements").readNullable[List[String]])(QuestionBody.apply _)
    }

    def asHtml(qb: Option[QuestionBody]): String = {

      def statementTag(v: String) = s"""<div class="statement">$v</div>"""
      def statementContainer(st: String) = """<div class="statement-container"><ul class="list-inline"><li>""" + st + """</li></ul></div>"""
      def label(l: String) = s"""<div class="label">$l</div>"""

      qb match {
        case Some(q) =>
          val s = q.statements match {
            case Some(s) => statementContainer((s map { x => statementTag(x) }).mkString("</li><li>"))
            case None => ""
          }
          label(q.label) + s
        case _ => label("") + statementContainer(statementTag(""))
      }

    }

  }

  case class DICTQ(qid: String, body: String, answer: String) {
    def item(instruction: String): Item = DICTI(uuid, QuestionBody(instruction, Some(List(body))), answer)
    def reverseItem(reverseInstruction: String): Item = DICTI(uuid, QuestionBody(reverseInstruction, Some(List(answer))), body)
    def asHtml: String = DICTQ.asHtml(Some(body), Some(answer))
  }

  object DICTQ {

    implicit val reader: Reads[DICTQ] = {
      (
        (__ \ "qid").read[String] and
        (__ \ "body").read[String] and
        (__ \ "answer").read[String])(DICTQ.apply _)
    }

    // implicit val readerWithoudId: Reads[DICTQ] = {
    //   (
    //     (__ \ "body").read[String] and
    //     (__ \ "answer").read[String])(DICTQ.apply (uuid,_,_))
    // }

    def bodyAsHtml(body: Option[String]): String = s"""<div class="body"><input type="text" class="form-control" id="question" placeholder="${body.getOrElse("")}"></div>"""
    def answerAsHtml(answer: Option[String]) = s"""<div class="answer"><input type="text" class="form-control" id="answer" placeholder="${answer.getOrElse("")}"></div>"""
    def asHtml(body: Option[String], answer: Option[String]): String = bodyAsHtml(body) + answerAsHtml(answer)
    def form: String = asHtml(None, None)

  }

  /**
   *
   * Group contains instruction for list of dictionaire items
   */
  case class DICTQGroup(qid: String, instruction: String, reverseInstruction: Option[String], questions: Stream[DICTQ]) {
    def items(reverse: Boolean): Stream[Item] =
      if (reverse) reverseInstruction match {
        case Some(ri) => questions map { _.reverseItem(ri) }
        case _ => Stream.Empty
      }
      else questions map { _.item(instruction) }

    def asHtml: String = DICTQGroup.asHtml(Some(instruction), reverseInstruction, questions)

  }

  object DICTQGroup {

    import DICTQ._

    implicit val reader: Reads[DICTQGroup] = {
      (
        (__ \ "qid").read[String] and
        (__ \ "instruction").read[String] and
        (__ \ "reverseInstruction").readNullable[String] and
        (__ \ "questions").read[Stream[DICTQ]])(DICTQGroup.apply _)
    }

    def asHtml(instruction: Option[String], reverseInstruction: Option[String], questions: Stream[DICTQ]): String = {
      def emptyQ = DICTQ.asHtml(None, None)
      def container(content: String) =
        s"""<div class="instruction">${instruction.getOrElse("")}</div>""" +
          s"""<div class="reverseInstruction">${reverseInstruction.getOrElse("")}</div>""" +
          s"""<div class="item-container">""" + content + s"""</div>"""

      questions match {
        case Stream.Empty => container(emptyQ)
        case _ => container((questions map { _.asHtml }).mkString(""))
      }

    }
  }

  /**
   *
   * Multiple Choice Ordered Question
   *
   */
  case class MCOQ(qid: String, body: QuestionBody, alternatives: List[String], minimum: Int) extends Question {
    def item: Item = MCOI(uuid, body, alternatives, minimum)
    def asHtml: String = MCOQ.asHtml(Some(body), Some(alternatives), minimum)
  }

  object MCOQ {
    implicit val reader: Reads[MCOQ] = {
      (
        (__ \ "qid").read[String] and
        (__ \ "body").read[QuestionBody](questionBodyReader) and
        (__ \ "alternatives").read[List[String]] and
        (__ \ "minimum").read[Int])(MCOQ.apply _)
    }

    // implicit val readerWithoudId: Reads[MCOQ] = {
    //   (
			 //  (__ \ "body").read[QuestionBody](questionBodyReader) and
			 //  (__ \ "alternatives").read[List[String]] and
			 //  (__ \ "minimum").read[Int])(MCOQ.apply (uuid,_,_,_))
    // }

    def asHtml(body: Option[QuestionBody], alternatives: Option[List[String]], minimum: Int): String = {

      def alternativeTag(as: Option[String]) = {
        s"""<div class="alternative">${as.getOrElse("")}</div>"""
      }

      def alternativeGroup(asg: Option[List[String]]) =
        asg match {
          case Some(a) => """<div class="alternative-container">""" +
            (a map { x => alternativeTag(Some(x)) }).mkString("") +
            """</div>"""
          case _ => """<div class="alternative-container"><ol><li>""" +
            alternativeTag(None) +
            """</li></ol></div>"""
        }

      QuestionBody.asHtml(body) + alternativeGroup(alternatives)

    }
  }

  /**
   *
   * Multiple Choice Unordered Question
   *
   */
  case class MCUQ(qid: String, body: QuestionBody, alternatives: Map[String, Boolean], minimum: Int) extends Question {
    def item: Item = MCUI(uuid, body, alternatives, minimum)
    def asHtml: String = MCUQ.asHtml(Some(body), Some(alternatives), minimum)
  }

  object MCUQ {
    implicit val reader: Reads[MCUQ] = {
      (
        (__ \ "qid").read[String] and
        (__ \ "body").read[QuestionBody](questionBodyReader) and
        (__ \ "alternatives").read[Map[String, Boolean]] and
        (__ \ "minimum").read[Int])(MCUQ.apply _)
    }
    
    // implicit val readerWithoutId: Reads[MCUQ] = {
    //   (
    //     (__ \ "body").read[QuestionBody](questionBodyReader) and
    //     (__ \ "alternatives").read[Map[String, Boolean]] and
    //     (__ \ "minimum").read[Int])(MCUQ.apply (uuid,_,_,_))
    // }
        
    def asHtml(body: Option[QuestionBody], alternatives: Option[Map[String, Boolean]], minimum: Int): String = {

      def alternativeTag(a: Option[String], b: Option[Boolean]) = s"""<div class="alternative">${a.getOrElse("")}</div><div class="correct">${b.getOrElse("")}</div>"""

      def alternativeGroup = alternatives match {
        case Some(asg) =>
          """<div class="alternative-container">""" +
            (asg map { x => alternativeTag(Some(x._1), Some(x._2)) }).mkString("") +
            """</div>"""
        case _ => """<div class="alternative-container"><ul><li>""" +
          alternativeTag(None, None) +
          """</li></ul></div>"""
      }
      QuestionBody.asHtml(body) + alternativeGroup

    }
  }

}

/**
 * Items are derived from questions but are answered and have a score
 */
sealed trait Item {
  import Item._
  import Question.QuestionBody

  def iid: String
  def body: QuestionBody
  def label: String = body.label

  def asHtml: String

  def trialMode: Boolean = false

  def isCorrect(a: Answers): Validation[String, Item]
}

object Item {

  import Question.QuestionBody
  import ErrorHandling._
  import Rehearsals.ItemResult
  
  def clean(s: String): String = s.replaceAll("\\s+", " ").trim

  def byId(id: String): Item = ???

  def unionV[I<:Item](a: Validation[String, I],b: Validation[String, I]): Validation[String, I] = (a,b) match {
    case (Success(a),Success(b)) => Success(a)
    case (Failure(a), Failure(b)) => Failure(a + "~" + b)
    case (Failure(a), _) => Failure(a)
    case (_, Failure(b)) => Failure(b)
  }

  case class AnswerResult[+A](q: A, r: Validation[String,A])
  /**
   * Dictionnary Item is bidirectional
   */
  case class DICTI(iid: String, body: QuestionBody, answer: String) extends Item {
    import Question.DICTQ

    val strict: Boolean = true

    def isCorrect(a: Answers):Validation[String,Item] =
      if (strict) {
        if (clean(answer) == clean(a.input.head)) correctAnswer(this)
        else wrongAnswer(iid, clean(answer))
      } else {
        if (clean(a.input.head).toLowerCase() == answer.toLowerCase()) correctAnswer(this)
        else wrongAnswer(iid, clean(answer))
      }

    def asHtml = QuestionBody.asHtml(Some(body)) + DICTQ.answerAsHtml(Some(answer))

  }

  object DICTI {

  }

  /**
   * Multiple Choice Ordered Item
   */
  case class MCOI(iid: String, body: QuestionBody, alternatives: List[String], minimum: Int) extends Item {
    import Question.MCOQ

    private def answerCompare(answer: String, correct: String) =
      if (clean(answer) == correct) Success(this)
      else wrongAnswer(iid, answer)

    //@tailrec
    private def evaluate(as: List[String], corrects: List[String]): Validation[String, MCOI] = {
      (as zip corrects)
        .take(minimum)
        .map(p => answerCompare(p._1, p._2))
        .fold(Success(this))((a,b) => Item.unionV(a,b))
    }

    def isCorrect(a: Answers):Validation[String,Item] =
      if (a.input.length < minimum) tooFewAnswers(iid, minimum)
      else { evaluate(a.input, alternatives) }

    def asHtml = MCOQ.asHtml(Some(body), Some(alternatives), minimum)
  }

  object MCOI {

  }

  /**
   * Multiple Choice Unordered Item
   */
  case class MCUI(iid: String, body: QuestionBody, alternatives: Map[String, Boolean], minimum: Int) extends Item {
    import Question.MCUQ

    private def evaluate(a: String): Validation[String, Item] = {
      val ca = clean(a)
      alternatives.get(ca) match {
        case Some(bb) => if (bb) correctAnswer(this) else wrongAnswer(iid, ca)
        case _ => answerNotAvailable(iid)
      }
    }

    def isCorrect(a: Answers) =
      if (a.input.length < minimum) tooFewAnswers(iid, minimum)
      else (a.input map { evaluate(_) }).fold(Success(this))((a,b) => Item.unionV(a,b))

    def asHtml: String = MCUQ.asHtml(Some(body), Some(alternatives), minimum)

  }

  object MCUI {

  }



  object ErrorHandling {
    def answerNotAvailable(id: String) = Failure("answerNotAvailable")
    def tooFewAnswers(id: String, minimum: Int) = Failure("""tooFewAnswers""")
    def wrongAnswer(id: String, answer: String) = Failure("""wrongAnswer""")
    def correctAnswer(i:Item) = Success(i)
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

