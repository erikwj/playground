package pool

object ResultItems {

  case class Answer[A](q: Stream[A], r: Boolean) {
    require(!q.isEmpty, q.length == 1)
  }

  sealed abstract class Pool[+A, -B] {
    import Pool._

    def qs: A
    def result[A,B>:A]: Result[A, B] = Result.Empty.apply

    //    def empty: A
    //
    //    def u[B>:A](q: A): B
    //
    //    def add[B](qs: A,q: B): B
    //
    //    def drop[B](qs: A, q: B):B
    //
    //    def contains[B>:A](qs: A,q:B): Boolean

    //    /**
    //     * Update: updates a Pool with an answers and returns a new Pool
    //     */
    //    def update[B>:A](b: Boolean)(q: B)(f: (B, Result[A]) => Pool[B]): Pool[B] = {
    //
    //      def updateA[B](q: B, t: Stream[Pool[A]])(f: (B, Result[A]) => Pool[B]): Stream[Pool[B]] = t match {
    //        case Stream.Empty => Stream(f(q, Result.Empty.apply))
    //        case _ => Stream(f(q, t.head.result))
    //      }
    //
    //      def updateCorrect(p: Stream[Pool[A]])(b: Boolean)(q: B)(f: (B, Result[A]) => Pool[B]): Stream[Pool[B]] =
    //        if (b) updateA(q, p)(f) else Stream(f(p.head.qs, p.head.result))
    //
    //      def updateInCorrect(p: Stream[Pool[A]])(b: Boolean)(q: B)(f: (B, Result[A]) => Pool[B]): Stream[Pool[B]] =
    //        if (!b) updateA(q, p)(f) else Stream(f(p.head.qs, p.head.result))
    //
    //      this match {
    //        case tc: Correct[A] => correct(drop(qs,q), Result(updateInCorrect(tc.result.left)(b)(q)(f), updateCorrect(tc.result.right)(b)(q)(f)))
    //        case ti: InCorrect[A] => incorrect(drop(qs,q), Result(updateInCorrect(ti.result.left)(b)(q)(f), updateCorrect(ti.result.right)(b)(q)(f)))
    //      }
    //    }

    //    def updater(b:Boolean) = update(b) _
    //    
    //    def updateBranch(a:Answer[A]) = {
    //      val u = updater(a.r)
    //      this flatMap {p => u(a.q)}
    //    }
    //    

    //    private val z: Stream[A] = Stream.Empty
    //
    //    private def someQs(p: Stream[Pool[A]]): A = p match {
    //      case Stream.Empty => Stream.Empty
    //      case _ => p.head.qs
    //    }
    //
    //    def levels: Stream[A] = fold(someQs)(_.append(_))

    //    def next: Stream[A] = folder(Stream(this))(z)((p: Pool[A], s: Stream[A]) => s.append(p.qs))(x => !x.isEmpty)

    //    def next = traverse(x => !x.isEmpty)

    //    def find(q: A): Pool[A] = finder(Stream(this))((p: Pool[A]) => p)(_.contains(q))

    //    def diff(p1: Pool[A], p2: Pool[A]) = ???
    //    def corrects: Int = ???
    //    def incorrects: Int = ???

    //    /**
    //     *
    //     * Map
    //     *
    //     */
    //    def map[B](f: A => B): Pool[A,B] = {
    //
    //      this match {
    //        case ti: InCorrect[A,B] => Pool.incorrect(f(ti.qs), Result(ti.result.left map (p => p map f), ti.result.right map (_ map f)))
    //        case tc: Correct[A,B] => Pool.correct(f(tc.qs), Result(tc.result.left map (_ map f), tc.result.right map (_ map f)))
    //      }
    //    }

    //    /**
    //     *
    //     * flatMap
    //     *
    //     */
    //    def flatMap[B](f: A => Pool[A,B]): Pool[A,B] = {
    //      this match {
    //        case ti: InCorrect[A,B] =>
    //          val r: Pool[A,B] = f(qs)
    //          Pool.incorrect(r.qs, Result((result.left map (_.flatMap(f))), (result.right map (_.flatMap(f)))))
    //        case tc: Correct[A,B] =>
    //          val r: Pool[A,B] = f(qs)
    //          Pool.correct(r.qs, Result((result.left map (_.flatMap(f))), (result.right map (_.flatMap(f)))))
    //      }
    //    }

    //    /**
    //     * Fold
    //     */
    //
    //    def fold[B](f: Stream[Pool[A,B]] => B)(g: (B, B) => B): B = {
    //      def foldL[B](t: Option[InCorrect[A,B]])(f: Stream[Pool[A]] => B)(g: (B, B) => B): B = t match {
    //        case Some(p) => folds[B, InCorrect[A]](p)(f)(g)
    //        case _ => f(Stream.Empty)
    //      }
    //
    //      def foldR[B](t: Option[Correct[A]])(f: Stream[Pool[A]] => B)(g: (B, B) => B): B = t match {
    //        case Some(p) => folds[B, Correct[A]](p)(f)(g)
    //        case _ => f(Stream.Empty)
    //      }
    //
    //      def opsI(p: Option[Pool[A]]): Option[InCorrect[A]] = p map {
    //        case i: InCorrect[A] => i
    //      }
    //
    //      def opsC(p: Option[Pool[A]]): Option[Correct[A]] = p map {
    //        case c: Correct[A] => c
    //      }
    //
    //      def folds[B, T <: Pool[A]](t: T)(f: Stream[Pool[A]] => B)(g: (B, B) => B): B =
    //        t.result.flatten.toList match {
    //          case Nil => f(Stream(t))
    //          case List(x: InCorrect[A], y: Correct[A]) =>
    //            val r = t.result
    //            g(foldL(opsI(r.left.headOption))(f)(g), foldR(opsC(r.right.headOption))(f)(g))
    //          case List(c: Correct[A]) =>
    //            g(f(Stream(this)), foldR(Some(c))(f)(g))
    //          case List(i: InCorrect[A]) =>
    //            g(f(Stream(this)), foldL(Some(i))(f)(g))
    //        }
    //
    //      this match {
    //        case i: InCorrect[A] => foldL(Some(i))(f)(g)
    //        case c: Correct[A] => foldR(Some(c))(f)(g)
    //      }
    //    }
    //
    //    def depth = this.fold(_ => 1)((d1, d2) => 1 + (d1 max d2))

    //    def leafs: Stream[(A, String)] = {
    //      type B = Stream[(A, String)]
    //
    //      def counter(p: Stream[Pool[A]]): B = {
    //        p match {
    //          case Stream.Empty => Stream((empty, ""))
    //          case _ => Stream((p.head.qs, p.head.show))
    //        }
    //      }
    //      this.fold(counter(_))(_.append(_))
    //    }

    /**
     *
     *
     *
     *
     */
    def show[C] = this match {
      case x: InCorrect[A, B] => "<I>"
      case x: Correct[A, B] => "<C>"
    }

  }

  case class InCorrect[A,  B >: A](qs: A, r: Result[A, B]) extends Pool[A,B]

  case class Correct[A,  B >: A](qs: A, r: Result[A, B]) extends Pool[A,B]

  case class Result[A, B >: A](left: Stream[Pool[A,B]], right: Stream[Pool[A,B]]) {
    def flatten[A] = Stream(this.left, this.right).flatten
  }

  object Result {
    object Empty {
      import pool.ResultItems.Pool._
      def apply[A,B >: A] = {
        val lStream: Stream[Pool[A, B]] = Stream.Empty
        val rStream: Stream[Pool[A, B]] = Stream.Empty
        Result(lStream, rStream)
      }
    }
//    def merge[A, B, C](a: Result[A, B, C], b: Result[A, B, C]) = Result(Stream(a.left, b.left).flatten, Stream(a.right, b.right).flatten)
  }

  object Pool {
    import Result._

    def incorrect[A,  B >: A](qs: A, r: Result[A, B]): Pool[A, B] = InCorrect(qs, r)
    def correct[A,  B >: A](qs: A, r: Result[A, B]): Pool[A, B] = Correct(qs, r)

//    def apply[A, B](qs: => A): Pool[A, B] = incorrect(qs, Result.Empty.apply)

  }

}