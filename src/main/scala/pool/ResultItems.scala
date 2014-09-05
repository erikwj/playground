package pool

object ResultItems {

  case class Answer[A](q: A, r: Boolean)

  sealed abstract class Pool[A] {
    import Pool._

    def qs: Stream[A]
    def result: Result[A]

    def dropQ(q: A): Stream[A] = qs.filter(_ != q)

    def appendQ(q: A) = qs.append(Stream(q))

    /**
     * Update: updates a Pool with an answers and returns a new Pool
     */
        def update(a: Answer[A]): Pool[A] = {
    
          def updateA[T <: Pool[A]](q: A, t: Stream[T])(f: (Stream[A], Result[A]) => Stream[T]) = t match {
            case Stream.Empty => f(Stream(q), Result.Empty.apply)
            case _ => f(t.head.appendQ(q), t.head.result)
          }
    
          def updateCorrect(p: Stream[Pool[A]], a: Answer[A]): Stream[Pool[A]] =
            if (a.r) updateA(a.q, p)((q, r) => Stream(correct(q, r))) else p
            
          def updateInCorrect(p: Stream[Pool[A]], a: Answer[A]): Stream[Pool[A]] =
            if (!a.r) updateA(a.q, p)((q,r) => Stream(incorrect(q, r))) else p
    
          if (contains(a.q)) this match {
            case tc: Correct[A] => correct(dropQ(a.q), Result(updateInCorrect(tc.result.left, a), updateCorrect(tc.result.right, a)))
            case ti: InCorrect[A] => incorrect(dropQ(a.q), Result(updateInCorrect(ti.result.left, a), updateCorrect(ti.result.right, a)))
          }
          else this
    
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
          case _ => Stream((p.head.qs, p.head.toString()))
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
    override def toString = this match {
      case x: InCorrect[A] => "<I>"
      case x: Correct[A] => "<C>"
    }

  }

  case class InCorrect[A](qs: Stream[A], result: Result[A]) extends Pool[A]

  case class Correct[A](qs: Stream[A], result: Result[A]) extends Pool[A]

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
  }

  object Pool {
    import Result._

    def incorrect[A](qs: Stream[A], r: Result[A]): Pool[A] = InCorrect(qs, r)
    def correct[A](qs: Stream[A], r: Result[A]): Pool[A] = Correct(qs, r)

    def apply[A](qs: => Stream[A]): Pool[A] = incorrect(qs, Result.Empty.apply)

    //    def folder[A, B](ps: Stream[Pool[A]])(z: B)(f: (Pool[A], B) => B)(g: B => Boolean): B = {
    //      if (ps.isEmpty) sys.error("ps should contain elements")
    //      if (g(z)) z
    //      else {
    //        val acc = ps.tail //todo list
    //        val h = ps.head //current element
    //        val hqs = f(h, z) //result stream
    //        h.result match {
    //          case Result(Stream.Empty, Stream.Empty) => //Leaf
    //            if (acc.isEmpty) hqs else folder(acc)(hqs)(f)(g)
    //          case _ => //Some node
    //            folder(acc.append(h.result.flatten))(hqs)(f)(g)
    //        }
    //      }
    //    }

    //    /**
    //     *
    //     */
    //    def finder[A, B](ps: Stream[Pool[A]])(f: (Pool[A]) => B)(g: B => Boolean): B = {
    //      if (ps.isEmpty) sys.error("ps should contain elements")
    //      else {
    //        val acc = ps.tail
    //        val h = ps.head
    //        val hqs = f(h)
    //        if (g(hqs)) hqs
    //        h.result match {
    //          case Result(Stream.Empty, Stream.Empty) => //Leaf
    //            if (acc.isEmpty) hqs else finder(acc)(f)(g)
    //          case _ => //Some node
    //            finder(acc.append(h.result.flatten))(f)(g)
    //        }
    //      }
    //    }

  }

}