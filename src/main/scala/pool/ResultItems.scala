package pool

object ResultItems {

  case class Answer[A](q: A, r: Boolean)

  sealed abstract class Pool[A] {
    import Pool._
    import Result._

    def qs: Stream[A]
    def result: Result[A]

    def dropQ(q: A): Stream[A] = qs.filter(_ != q)

    def appendQ(q: A) = qs.append(Stream(q))

    private def updateA[T <: Pool[A]](q: A, t: Option[T])(f: (Stream[A], Result[A]) => Option[T]) = t match {
      case Some(p) => f(p.appendQ(q), p.result)
      case None => f(Stream(q), Result(None, None))
    }

    private def updateCorrect(p: Option[Correct[A]], a: Answer[A]): Option[Correct[A]] =
      if (a.r) updateA(a.q, p)((a, b) => Some(Correct(a, b))) else p
    private def updateInCorrect(p: Option[InCorrect[A]], a: Answer[A]): Option[InCorrect[A]] =
      if (!a.r) updateA(a.q, p)((a, b) => Some(InCorrect(a, b))) else p

    def update(a: Answer[A]) = if (contains(a.q)) this match {
      case tc: Correct[A] => Correct(dropQ(a.q), Result(updateInCorrect(tc.result.left, a), updateCorrect(tc.result.right, a)))
      case ti: InCorrect[A] => InCorrect(dropQ(a.q), Result(updateInCorrect(ti.result.left, a), updateCorrect(ti.result.right, a)))
    }
    else this

//    def next = finder(Stream(this))((p:Pool[A]) => p.qs)(!_.isEmpty)

    def isHead(q: A): Boolean = qs.head == q
    def contains(q: A): Boolean = qs.contains(q)
    
    val emptyStream:Stream[A] = Stream.Empty
    def levels: Stream[A] = fold(Stream(this))(emptyStream)((p:Pool[A],s:Stream[A]) => s.append(p.qs))(_ => false)
    def next: Stream[A] = fold(Stream(this))(emptyStream)((p:Pool[A],s:Stream[A]) => s.append(p.qs))(x => !x.isEmpty)

    def find(q:A) =
      finder(Stream(this))((p:Pool[A]) => p)(_.contains(q))
    

    def diff(p1: Pool[A], p2: Pool[A]) = ???
    def depth = ???
    def corrects: Int = ???
    def incorrects: Int = ???
  }
  

  case class InCorrect[A](qs: Stream[A], result: Result[A]) extends Pool[A]

  case class Correct[A](qs: Stream[A], result: Result[A]) extends Pool[A]

  case class Result[A](left: Option[InCorrect[A]], right: Option[Correct[A]]) {

    def flatten: Stream[Pool[A]] = Stream(this.left, this.right).flatten
  }

  object Result {
    //	  def flatten[A](r:Result[A]):Stream[Pool[A]] = Stream(r.left,r.right).flatten

  }

  object Pool {

      def fold[A,B](ps: Stream[Pool[A]])(res:B)(f:(Pool[A], B) => B)(g:B => Boolean): B = {
        if (ps.isEmpty) sys.error("ps should contain elements")
        if(g(res)) res
        else {
          val acc = ps.tail //todo list
          val h = ps.head //current element
          val hqs = f(h,res) //result stream
          h.result match {
            case Result(None, None) => //Leaf
              if (acc.isEmpty) hqs else fold(acc)(hqs)(f)(g)
            case _ => //Some branch
              fold(acc.append(h.result.flatten))(hqs)(f)(g)
          }
        }
      }
      
      def finder[A,B](ps: Stream[Pool[A]])(f:(Pool[A]) => B)(g:B => Boolean): B = {
    		  if (ps.isEmpty) sys.error("ps should contain elements")
    		  else {
    			  val acc = ps.tail
    					  val h = ps.head
    					  val hqs = f(h)
    					  if(g(hqs)) hqs
    					  h.result match {
    					  case Result(None, None) => //Leaf
    					  if (acc.isEmpty) hqs else finder(acc)(f)(g)
    					  case _ => //Some branch
    					  finder(acc.append(h.result.flatten))(f)(g)
    			  }
    		  }
      }
      
//      def map[A,B](p: Option[Pool[A]])(f: Stream[A] => Stream[B]):Pool[B] = p match {
//        case Some(t) => t match {
//        case InCorrect(v,Result(a,b)) => InCorrect(f(v),Result(map(a.asInstanceOf[Option[Pool[A]]])(f),map(b.asInstanceOf[Option[Pool[A]]])(f)))
//      }
//      }
//      
  }


}