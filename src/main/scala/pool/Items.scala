package pool

object Items {

  type Question = Int

  case class Answer(q: Question, r: Boolean)

  sealed abstract class Pool {
    import Pool._
    
    def qs: Stream[Question]
    def c: Option[Correct]
    def i: Option[InCorrect]

    def stripQuestion(q: Question): Stream[Question] = qs.filter(_ != q)

    def appendQ(q: Question) = qs.append(Stream(q))

    private def updateNode[T <: Pool](ic: Option[T], q: Question)(f: (Stream[Question], Option[InCorrect], Option[Correct]) => T): T = ic match {
      case Some(c) => f(c.appendQ(q), c.i, c.c)
      case _ => f(Stream(q), None, None)
    }

    private def updateCorrect(c: Option[Correct], a: Answer): Option[Correct] = if(a.r) Some(updateNode[Correct](c, a.q)(Correct(_, _, _))) else c
    private def updateInCorrect(c: Option[InCorrect], a: Answer): Option[InCorrect] = if(!a.r) Some(updateNode[InCorrect](c, a.q)(InCorrect(_, _, _))) else c

    def update(a: Answer):Pool = this match {
      case tc:Correct => Correct(stripQuestion(a.q), updateInCorrect(tc.i, a), updateCorrect(tc.c, a))
      case ti:InCorrect => InCorrect(stripQuestion(a.q), updateInCorrect(ti.i, a), updateCorrect(ti.c, a))
    }
    
    def next:Question = ???
    
    def isHead(q:Question):Boolean = qs.head == q
    def contains(q:Question):Boolean = qs.contains(q)
    
//    def map[B](f: A => B): Tree[B] =
//      node(f(rootLabel), subForest map (_ map f))

    def traverse:Stream[Pool] = this match {
      case InCorrect(_,Some(i),Some(c)) => Stream(i,c)
      case InCorrect(_,None,Some(c)) => Stream(c)
      case InCorrect(_,Some(i),None) => Stream(i)
      case InCorrect(_,None,None) => Stream.Empty
      case Correct(_,Some(i),Some(c)) => Stream(i,c)
      case Correct(_,Some(i),None) => Stream(i)
      case Correct(_,None,Some(c)) => Stream(c)
      case Correct(_,None,None) => Stream.Empty
      case _ => Stream.Empty
    }
  }

  case class InCorrect(qs: Stream[Question], i: Option[InCorrect], c: Option[Correct]) extends Pool

  case class Correct(qs: Stream[Question], i: Option[InCorrect], c: Option[Correct]) extends Pool


  object Pool {
//    def apply[T <: Pool](questions: Stream[Question], incorrect: Option[InCorrect], correct: Option[Correct]) = new Pool {
//     lazy val qs: Stream[Question] = questions
//     lazy val i: Option[InCorrect] = incorrect
//     lazy val c: Option[Correct] = correct }
//    
//    

  }
  
  //State 0
  val s0 = InCorrect(Stream(1, 2, 3, 4, 5, 6), None, None)
  val a1: Answer = Answer(1,false)
  val a2: Answer = Answer(2,true)
  //State 1 => updated State 0 by Answer
  val sa = s0.update(a1)
//  val p = Pool.apply(sa._1, sa._2, sa._3)
  
//  Pool.create(s0, qs, i, )
  val s1: Pool = InCorrect(Stream(2, 3, 4, 5, 6), Some(InCorrect(Stream(1), None, None)), None)
  val s2 = InCorrect(Stream(3, 4, 5, 6), Some(InCorrect(Stream(1), None, None)), Some(Correct(Stream(2), None, None)))
  val s3 = InCorrect(Stream(4, 5, 6), Some(InCorrect(Stream(1, 3), None, None)), Some(Correct(Stream(2), None, None)))
  val s6 = InCorrect(Stream(), Some(InCorrect(Stream(1, 3), None, None)), Some(Correct(Stream(2, 4, 5, 6), None, None)))
  val s7 = InCorrect(Stream(), Some(InCorrect(Stream(3), Some(InCorrect(Stream(1), None, None)), None)), Some(Correct(Stream(2, 4, 5, 6), None, None)))

}