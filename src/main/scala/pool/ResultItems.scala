package pool

object ResultItems {

  case class Answer[A](q: Stream[A], r: Boolean){
    require(!q.isEmpty,q.length == 1)
  }

  sealed abstract class Pool[+A] {
    import Pool._

    def qs: A
    def result: Result[A]

    def add[B](q: B)(f:A => B):B 

    def drop[B](q: B)(f:A => B):B 
    
    def contains[B](q:B):Boolean

    /**
     * Update: updates a Pool with an answers and returns a new Pool
     */
        def update[B](b:Boolean)(a: B): Pool[B] = {
    
          def updateA[T <: Pool[A]](q: A, t: T)(f: (A, Result[A]) => T) = t match {
            case Stream.Empty => f(q, Result.Empty.apply)
            case _ => f(q, t.result)
          }
    
          def updateCorrect(p: Pool[A])(q:A)(b:Boolean): Pool[A] =
            if (b) updateA(q, p)((q, r) => correct(q, r)) else p
            
          def updateInCorrect(p: Pool[A])(q:A)(b:Boolean): Pool[A] =
            if (!b) updateA(q, p)((q,r) => incorrect(q, r)) else p
    
          if (contains(a)) this match {
            case tc: Correct[A] => correct(drop(this.qs,a.head), Result(updateInCorrect(tc.result.left)(a)(b), updateCorrect(tc.result.right)(a)(b)))
            case ti: InCorrect[A] => incorrect(drop(this.qs,a.head), Result(updateInCorrect(ti.result.left)(a)(b), updateCorrect(ti.result.right)(a)(b)))
          }
          else this
    
        }
        
    def updater(b:Boolean) = update(b) _
    
    def updateBranch(a:Answer[A]) = {
      val u = updater(a.r)
      this flatMap {p => u(a.q)}
    }
    
    def isHead(q: A): Boolean = qs.head == q
    def contains(q: A): Boolean = qs.contains(q)

    /**
     * Type setting needed to convince the compiler that it's a A stream
     */
    private val z: Stream[A] = Stream.Empty

    private def someQs(p: Stream[Pool[A]]): Stream[A] = p match {
      case Stream.Empty => Stream.Empty
      case _ => p.head.qs
    }

    def levels: Stream[A] = fold(someQs)(_.append(_))

    //    def next: Stream[A] = folder(Stream(this))(z)((p: Pool[A], s: Stream[A]) => s.append(p.qs))(x => !x.isEmpty)

    //    def next = traverse(x => !x.isEmpty)

    //    def find(q: A): Pool[A] = finder(Stream(this))((p: Pool[A]) => p)(_.contains(q))

    def diff(p1: Pool[A], p2: Pool[A]) = ???
    def corrects: Int = ???
    def incorrects: Int = ???

    /**
     *
     * Map
     *
     */
    def map[B](f: Stream[A] => Stream[B]): Pool[B] = {

      this match {
        case ti: InCorrect[A] => Pool.incorrect(f(ti.qs), Result(ti.result.left map (p => p map f), ti.result.right map (_ map f)))
        case tc: Correct[A] => Pool.correct(f(tc.qs), Result(tc.result.left map (_ map f), tc.result.right map (_ map f)))
      }
    }

    /**
     *
     * flatMap
     *
     */
    def flatMap[B](f: Stream[A] => Pool[B]): Pool[B] = {
      this match {
        case ti: InCorrect[A] =>
          val r: Pool[B] = f(qs)
          Pool.incorrect(r.qs, Result((result.left map (_.flatMap(f))), (result.right map (_.flatMap(f)))))
        case tc: Correct[A] =>
          val r: Pool[B] = f(qs)
          Pool.correct(r.qs, Result((result.left map (_.flatMap(f))), (result.right map (_.flatMap(f)))))
      }
    }

    /**
     * Fold
     */

    def fold[B](f: Stream[Pool[A]] => B)(g: (B, B) => B): B = {
      def foldL[B](t: Option[InCorrect[A]])(f: Stream[Pool[A]] => B)(g: (B, B) => B): B = t match {
        case Some(p) => folds[B, InCorrect[A]](p)(f)(g)
        case _ => f(Stream.Empty)
      }

      def foldR[B](t: Option[Correct[A]])(f: Stream[Pool[A]] => B)(g: (B, B) => B): B = t match {
        case Some(p) => folds[B, Correct[A]](p)(f)(g)
        case _ => f(Stream.Empty)
      }

      def opsI(p: Option[Pool[A]]): Option[InCorrect[A]] = p map {
        case i: InCorrect[A] => i
      }

      def opsC(p: Option[Pool[A]]): Option[Correct[A]] = p map {
        case c: Correct[A] => c
      }

      def folds[B, T <: Pool[A]](t: T)(f: Stream[Pool[A]] => B)(g: (B, B) => B): B =
        t.result.flatten.toList match {
          case Nil => f(Stream(t))
          case List(x: InCorrect[A], y: Correct[A]) =>
            val r = t.result
            g(foldL(opsI(r.left.headOption))(f)(g), foldR(opsC(r.right.headOption))(f)(g))
          case List(c: Correct[A]) =>
            g(f(Stream(this)), foldR(Some(c))(f)(g))
          case List(i: InCorrect[A]) =>
            g(f(Stream(this)), foldL(Some(i))(f)(g))
        }

      this match {
        case i: InCorrect[A] => foldL(Some(i))(f)(g)
        case c: Correct[A] => foldR(Some(c))(f)(g)
      }
    }

    def depth = this.fold(_ => 1)((d1, d2) => 1 + (d1 max d2))

    def leafs: Stream[(Stream[A], String)] = {
      type B = Stream[(Stream[A], String)]

      def counter(p: Stream[Pool[A]]): B = {
        p match {
          case Stream.Empty => Stream((Stream.Empty, ""))
          case _ => Stream((p.head.qs, p.head.show))
        }
      }
      this.fold(counter(_))(_.append(_))
    }

    /**
     *
     *
     *
     *
     */
    def show = this match {
      case x: InCorrect[A] => "<I>"
      case x: Correct[A] => "<C>"
    }

  }

  case class InCorrect[A](qs: A, result: Result[A]) extends Pool[A]

  case class Correct[A](qs: A, result: Result[A]) extends Pool[A]

  case class Result[A](left: Stream[Pool[A]], right: Stream[Pool[A]]) {
    def flatten[A] = Stream(this.left, this.right).flatten
  }

  object Result {
    object Empty {
      import pool.ResultItems.Pool._
      def apply[A] = {
        val emptyStream: Stream[Pool[A]] = Stream.Empty
        Result[A](emptyStream, emptyStream)
      }
    }
    def merge[A](a:Result[A],b:Result[A]) = Result(Stream(a.left,b.left).flatten,Stream(a.right,b.right).flatten)
  }

  object Pool {
    import Result._

    def incorrect[A](qs: A, r: Result[A]): Pool[A] = InCorrect(qs, r)
    def correct[A](qs: A, r: Result[A]): Pool[A] = Correct(qs, r)

    def apply[A](qs: => A): Pool[A] = incorrect(qs, Result.Empty.apply)

  }

}