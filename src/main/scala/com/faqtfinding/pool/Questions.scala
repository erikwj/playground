package com.faqtfinding.pool

import play.api.libs.json._
import play.api.libs.json.Reads
import play.api.libs.functional.syntax._
import scalaz._
import scalaz.Scalaz._
//import scala.Stream

import com.faqtfinding.pool.HTTree.AnswerResult


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
  import com.faqtfinding.pool.HTTree.AnswerResult

  def clean(s: String): String = s.replaceAll("\\s+", " ").trim

  def toItemResult(items:Stream[Item]):Stream[ItemResult] = (items map { i => ItemResult(i,0,List.empty)}).toStream

  def byId(id: String): Item = ???

  def unionV[I<:Item](a: Validation[String, I],b: Validation[String, I]): Validation[String, I] = (a,b) match {
    case (Success(a),Success(b)) => Success(a)
    case (Failure(a), Failure(b)) => Failure(a + "~" + b)
    case (Failure(a), _) => Failure(a)
    case (_, Failure(b)) => Failure(b)
  }

  //    case class AnswerResult[+A](q: A, r: Validation[String,A])

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

  /*
  =======================================================================================
  */

  case class ItemResult(item:Item,score:Int,history:List[(String,Validation[String,Item])]) {
    
    private def scoreIncrease:Int = score + 1 //could be + 1

    /**
    *  Choose between 
    0  => a sequence of good answers is required to stop this question from popping up
    -1 => the total number of correct answers will stop this question from popping up
    -2 => every wrong answer needs an extra good one
    **/
    private def scoreDecrease1:Int = score - 1
    private def scoreDecreaseTotal:Int = 0
    private def updateHistory(ar:AnswerResult[Item]) = (ar.q.iid -> ar.r)::history //move only references keep items in Root?
    private def updater(score:Int,ar:AnswerResult[Item]) = this.copy(score = score, history = updateHistory(ar))

    def update(ar:AnswerResult[Item]):ItemResult = 
      if(item == ar.q) {
        ar.r match {
          case Success(_) => updater(scoreIncrease,ar)
          case Failure(_) => updater(scoreDecrease1,ar)
        } //move only references keep items in Root?
      } else {
        this
      }

    def answerHistory:List[(String,Validation[String,Item])] = history.reverse

  }

  object ItemResult {

    
  }



//     def next:Option[Zipper[ItemSet]] = {
//       //FILTER ITEMS that have been completed and put thos in history file
//       if(atEnd) Stream(ItemSet(lefts.toZipper, Stream.Empty,Stream.Empty),ItemSet(rights.toZipper, Stream.Empty,Stream.Empty)).toZipper
//       // if(current.atEnd) Stream(ItemSet( (lefts.append(rights).toZipper.get, Stream.Empty,Stream.Empty)).toZipper
//       else Stream(ItemSet(current map {_.next},lefts,rights)).toZipper
//     }


  case class ItemSet(items:Option[Zipper[ItemResult]], lefts:Stream[ItemResult],rights: Stream[ItemResult],stopCriterium:Int) {

// case class AnswerResult[+A](q: A, r: Validation[String,A])
    import ItemSet._

    def atEnd:Boolean = (items map {_.atEnd}).getOrElse(true) //at end if empty set

    //Should this be publically available?
    def next(lefts:Stream[ItemResult],rights: Stream[ItemResult]):ItemSet = {
      if(atEnd) itemset(filterFinishedItems(lefts.append(rights),stopCriterium),stopCriterium)
      else this.copy(items = items >>= {_.next},lefts,rights) 
    }

    //State change see chapter 6 fpis
    def update(answerResult:AnswerResult[Item]):ItemSet = {

      val newitemresult: Stream[ItemResult] = (focus map {_.update(answerResult)}) match {
          case Some(ir) => Stream(ir)
          case _ => Stream.Empty 
        }

      //update internal state and return next 
      answerResult.r match {
        case Success(_) => next(lefts,rights.append(newitemresult))
        case Failure(_) => next(lefts.append(newitemresult),rights)
      }

    }
    
    def focus:Option[ItemResult] = items map {_.focus}

    def isAnswered:Boolean = (focus map {(ir) => lefts.contains(ir) || rights.contains(ir)}).getOrElse(true)
    
    // def exam = Stream(this)
  }

  object ItemSet {

    def itemset(items:Stream[ItemResult],stopCriterium:Int): ItemSet = ItemSet(items.toZipper,Stream.Empty,Stream.Empty,stopCriterium)    

    def modifyItems(answerResult:AnswerResult[Item],items:Option[Zipper[ItemResult]]):Option[Zipper[ItemResult]] = {
      def updateItemResult(itemResult:ItemResult):ItemResult = itemResult.update(answerResult)
      items map {_.modify{updateItemResult}}
    }

    def filterFinishedItems(irs: Stream[ItemResult],stopCriterium:Int): Stream[ItemResult] = irs.filter( (ir) => ir.score < stopCriterium)

  }
  

  case class Rehearsal(itemset:Option[ItemSet],stopCriterium:Int) {
    require(stopCriterium >= 1)

    def items: Option[Zipper[ItemResult]] = itemset >>= {_.items}

    def lefts: Option[Stream[ItemResult]] = itemset map {_.lefts}

    def rights: Option[Stream[ItemResult]] = itemset map {_.rights}

    def focus: Option[ItemResult] = items map {_.focus}
    
    def item: Option[Item] = focus map {_.item}
    
    //MODIFY STATE PUSH ZIPPER TO NEXT POSITION AND RETURN NEW 
    // def next:Rehearsal = this.copy(itemset = itemset map {(iset) => iset.next(iset.lefts,iset.rights)})
    
    def update(ar:AnswerResult[Item]): Rehearsal = 
      if((item map {(i) => i == ar.q}).getOrElse(false)) this.copy(itemset = itemset map {_.update(ar)})
      else this

    def isAnswered:Boolean = (itemset map {_.isAnswered}).getOrElse(true)

    //UGLY!   
    def isFinished:Boolean = 
      (itemset map {_.items match {
        case None => true
        case _ => false
      }
    }).getOrElse(true)


  }

  object Rehearsal {
    import ItemSet._

    def rehearsal(items:Stream[ItemResult],stopCriterium:Int):Rehearsal = items match {
      case Stream.Empty => Rehearsal(None,stopCriterium)
      case _ => Rehearsal(Some(itemset(items,stopCriterium)),stopCriterium)    
    }
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

