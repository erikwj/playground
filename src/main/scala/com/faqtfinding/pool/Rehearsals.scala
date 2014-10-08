package com.faqtfinding.pool

import com.faqtfinding.pool.Item._
import scalaz._
import Scalaz._

object Rehearsals {

  /**
   * Some explanation
   *
   * This class should be abstract
   * By implementing the scoreDecrease function you can decide what kind of stopCriterium you want to support
   *
   */
  case class ItemResult(item: Item, score: Int, history: List[(String, Validation[String, Item])]) {

    import ItemResult._

    private def scoreIncrease: Int = score + 1 //could be + 1

    /**
     *  Choose between
     * 0  => a sequence of good answers is required to stop this question from popping up
     * -1 => the total number of correct answers will stop this question from popping up
     * -2 => every wrong answer needs an extra good one
     */
    private def scoreDecrease1: Int = score - 1
    private def scoreDecreaseTotal: Int = 0 //should be flexible

    private def updateHistory(ar: AnswerResult[Item]) =
      (ar.q.iid -> ar.r) :: history //move only references keep items in Root?

    private def updater(score: Int, ar: AnswerResult[Item]) =
      this.copy(score = score, history = updateHistory(ar))

    def update(ar: AnswerResult[Item]): ItemResult =
      if (item == ar.q) {
        ar.r match {
          case Success(_) => updater(scoreIncrease, ar)
          case Failure(_) => updater(scoreDecrease1, ar)
        } //move only references keep items in Root?
      } else {
        this
      }

    def answerHistory: List[(String, Validation[String, Item])] = history.reverse

    def clean = itemresult(item)

  }

  /**
   * State container for Item
   */
  object ItemResult {
    def itemresult(i: Item) = ItemResult(i, 0, List.empty)
    def toItemResult(items: Stream[Item]): Stream[ItemResult] = items.distinct map { itemresult(_) }
  }

  /**
   *
   * Different implementation
   * if(atEnd) Stream(ItemSet(lefts.toZipper, Stream.Empty,Stream.Empty),ItemSet(rights.toZipper, Stream.Empty,Stream.Empty)).toZipper
   *
   *
   */

  case class ItemSet(items: Option[Zipper[ItemResult]], incorrects: Stream[ItemResult], corrects: Stream[ItemResult], stopCriterium: Option[Int]) {

    import ItemSet._

    def atEnd: Boolean = (items map { _.atEnd }).getOrElse(true) //at end if empty set

    //Should this be publically available?
    private def next(incorrects: Stream[ItemResult], corrects: Stream[ItemResult]): ItemSet = {
      if (atEnd) itemset(filterFinishedItems(incorrects.append(corrects), stopCriterium), stopCriterium)
      else this.copy(items = items >>= { _.next }, incorrects, corrects)
    }

    //State change see chapter 6 fpis
    def update(answerResult: AnswerResult[Item]): ItemSet = {

      //refactor
      val newitemresult: Stream[ItemResult] = (focus map { _.update(answerResult) }) match {
        case Some(ir) => Stream(ir)
        case _ => Stream.Empty
      }

      //update internal state and return next 
      answerResult.r match {
        case Success(_) => next(incorrects, corrects.append(newitemresult))
        case Failure(_) => next(incorrects.append(newitemresult), corrects)
      }
    }

    def focus: Option[ItemResult] = items map { _.focus }

    def isAnswered(item: Option[ItemResult]): Boolean = ItemSet.isAnswered(item, incorrects, corrects)

    def deleteItem: Option[Zipper[ItemResult]] = items >>= { _.deleteRight }

    def delete: ItemSet = {
      val newItems = deleteItem
      if (isAnswered(newItems map { _.focus })) {
        itemset(filterFinishedItems(incorrects.append(corrects), stopCriterium), stopCriterium)
      } else this.copy(items = newItems, incorrects, corrects)
    }

  }

  object ItemSet {
    
    def itemset(zitems: Option[Zipper[ItemResult]], incorrects: Stream[ItemResult], corrects: Stream[ItemResult], stopCriterium: Option[Int]):ItemSet =
      ItemSet(zitems, incorrects, corrects, stopCriterium) 

    def itemset(zitems: Option[Zipper[ItemResult]], stopCriterium: Option[Int]):ItemSet =
      itemset(zitems, Stream.Empty, Stream.Empty, stopCriterium) 
    
    def itemset(items: Stream[ItemResult], stopCriterium: Option[Int]): ItemSet =
      itemset(zipper(items), stopCriterium)
    
    def itemset(lefts: Stream[ItemResult], focus: ItemResult, rights: Stream[ItemResult], stopCriterium: Option[Int]): ItemSet =
      itemset(zipper(lefts, focus, rights), stopCriterium)
    
    def itemset(lefts: Stream[ItemResult], focus: ItemResult, rights: Stream[ItemResult], incorrects: Stream[ItemResult], corrects: Stream[ItemResult], stopCriterium: Option[Int]): ItemSet =
      itemset(zipper(lefts, focus, rights), incorrects, corrects, stopCriterium)

    def modifyItems(answerResult: AnswerResult[Item], items: Option[Zipper[ItemResult]]): Option[Zipper[ItemResult]] = {
      def updateItemResult(itemResult: ItemResult): ItemResult = itemResult.update(answerResult)
      items map { _.modify { updateItemResult } }
    }

    def filterFinishedItems(irs: Stream[ItemResult], stopCriterium: Option[Int]): Stream[ItemResult] = 
      (stopCriterium map {(sc) => irs.filter(_.score < sc)}).getOrElse(irs)

    def isAnswered(itemresult: Option[ItemResult], incorrects: Stream[ItemResult], corrects: Stream[ItemResult]): Boolean = {
      def irContItem(i: Item, irs: Stream[ItemResult]): Boolean = irs.map(_.item).contains(i)
      (itemresult map { (ir) => irContItem(ir.item, incorrects) || irContItem(ir.item, corrects) }).getOrElse(true)
    }

    def zipper(items: Stream[ItemResult]): Option[Zipper[ItemResult]] = items.toZipper

    def zipper(lefts: Stream[ItemResult], focus: ItemResult, rights: Stream[ItemResult]): Option[Zipper[ItemResult]] =
      Some(Zipper(lefts, focus, rights))

  }

  case class Rehearsal(label: String, itemSet: Option[ItemSet], stopCriterium: Option[Int]) {
//    require((stopCriterium map {_ > 0}).getOrElse(true))
//    require((stopCriterium map {_ > 10}).getOrElse(true))
    require(label.length > 0)
    
    import Rehearsal._
    import ItemSet._

    def items: Option[Zipper[ItemResult]] = itemSet >>= { _.items }

    def incorrects: Stream[ItemResult] = (itemSet map { _.incorrects }).getOrElse(Stream.Empty)

    def corrects: Stream[ItemResult] = (itemSet map { _.corrects }).getOrElse(Stream.Empty)

    def focus: Option[ItemResult] = items map { _.focus }

    def lefts: Option[Stream[ItemResult]] = items map { _.lefts }

    def rights: Option[Stream[ItemResult]] = items map { _.rights }

    def item: Option[Item] = focus map { _.item }

    def update(ar: AnswerResult[Item]): Rehearsal =
      if ((item map { (i) => i == ar.q }).getOrElse(false)) copyItemSet(itemSet map { _.update(ar) })
      else this

    def isAnswered: Boolean = ItemSet.isAnswered(focus, incorrects, corrects)

    //UGLY!   
    def isFinished: Boolean =
      (itemSet map {
        _.items match {
          case None => true
          case _ => false
        }
      }).getOrElse(true)

    def length: Int = (items map { _.length }).getOrElse(0)

    def index: Int = (items map { _.index }).getOrElse(0)

    def deleteCurrentItem = copyItemSet(itemSet map { _.delete })

     /**
     * Cleans all history and score and puts pointer to start position. Overrides current stopCriterium
     */
    def reset(sc:Option[Int]): Rehearsal = {
      val iset = items map { _ map { _.clean } } map { _.start }
      copy(itemSet = Some(itemset(iset, sc)), stopCriterium = sc)
    }
    
    /**
     * Cleans all history and score and puts pointer to start position. Use current stopCriterium
     */
    def reset: Rehearsal = reset(stopCriterium)
    
    def copyItemSet(itemSet: Option[ItemSet]) = this.copy(itemSet = itemSet)
    
    def answerHistory = incorrects.append(corrects) map {_.answerHistory}
    
    def contains(question:Question):Boolean = 
      (items map {(z) => z.foldLeft(false)((b:Boolean,ir:ItemResult) => (b && ir.item.qid == question.qid))}).getOrElse(false)
    
  }

  object Rehearsal {
    import ItemSet._

    def rehearsal(label: String, itemSet: Option[ItemSet], stopCriterium: Option[Int]): Rehearsal =
      Rehearsal(label, itemSet, stopCriterium)
    def rehearsal(label: String, itemSet: Stream[ItemResult], stopCriterium: Option[Int]): Rehearsal =
      Rehearsal(label, Some(itemset(itemSet, stopCriterium)), stopCriterium)
  }

}