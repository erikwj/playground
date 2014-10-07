package com.faqtfinding.pool

import com.faqtfinding.pool.Item._
import scalaz._
import Scalaz._

object Rehearsals {

  /**
   * Some explanation
   * 
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
    private def scoreDecreaseTotal:Int = 0 //should be flexible
    
    private def updateHistory(ar:AnswerResult[Item]) = 
      (ar.q.iid -> ar.r)::history //move only references keep items in Root?
    
    private def updater(score:Int,ar:AnswerResult[Item]) = 
      this.copy(score = score, history = updateHistory(ar))

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
    def itemresult(i:Item) = ItemResult(i,0,List.empty)
    def toItemResult(items:Stream[Item]):Stream[ItemResult] = items.distinct map {itemresult(_)}
  }



  /**
   * 
   * Different implementation
   * if(atEnd) Stream(ItemSet(lefts.toZipper, Stream.Empty,Stream.Empty),ItemSet(rights.toZipper, Stream.Empty,Stream.Empty)).toZipper
   * 
   * 
   */

  case class ItemSet(items:Option[Zipper[ItemResult]], lefts:Stream[ItemResult],rights: Stream[ItemResult],stopCriterium:Int) {

    import ItemSet._

    def atEnd:Boolean = (items map {_.atEnd}).getOrElse(true) //at end if empty set


    //State change see chapter 6 fpis
    def update(answerResult:AnswerResult[Item]):ItemSet = {

      //Should this be publically available?
      def next(lefts:Stream[ItemResult],rights: Stream[ItemResult]):ItemSet = {
        if(atEnd) itemset(filterFinishedItems(lefts.append(rights),stopCriterium),stopCriterium)
        else this.copy(items = items >>= {_.next},lefts,rights) 
      }

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

  }

  object ItemSet {

    def itemset(items:Stream[ItemResult],stopCriterium:Int): ItemSet = ItemSet(items.toZipper,Stream.Empty,Stream.Empty,stopCriterium)    

    def modifyItems(answerResult:AnswerResult[Item],items:Option[Zipper[ItemResult]]):Option[Zipper[ItemResult]] = {
      def updateItemResult(itemResult:ItemResult):ItemResult = itemResult.update(answerResult)
      items map {_.modify{updateItemResult}}
    }

    def filterFinishedItems(irs: Stream[ItemResult],stopCriterium:Int): Stream[ItemResult] = 
      irs.filter(_.score < stopCriterium)

  }
  

  case class Rehearsal(label:String, itemset:Option[ItemSet],stopCriterium:Int) {
    require(stopCriterium > 0)
    require(stopCriterium < 10)
    require(label.length > 0)

    def items: Option[Zipper[ItemResult]] = itemset >>= {_.items}

    def lefts: Option[Stream[ItemResult]] = itemset map {_.lefts}

    def rights: Option[Stream[ItemResult]] = itemset map {_.rights}

    def focus: Option[ItemResult] = items map {_.focus}
    
    def item: Option[Item] = focus map {_.item}
    
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

   def length:Int = (itemset flatMap {_.items map {_.toStream.length}}).getOrElse(0)

  }

  object Rehearsal {
    import ItemSet._

    def rehearsal(label:String, items:Stream[ItemResult],stopCriterium:Int):Rehearsal = items match {
      case Stream.Empty => Rehearsal(label, None,stopCriterium)
      case _ => Rehearsal(label, Some(itemset(items,stopCriterium)),stopCriterium)    
    }
  }

}