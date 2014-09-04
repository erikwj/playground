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

    /**
     * Update: updates a Pool with an answers and returns a new Pool
     */
    def update(a: Answer[A]): Pool[A] = {

      def updateA[T <: Pool[A]](q: A, t: Option[T])(f: (Stream[A], Result[A]) => Option[T]) = t match {
        case Some(p) => f(p.appendQ(q), p.result)
        case None => f(Stream(q), Result(None, None))
      }

      def updateCorrect(p: Option[Correct[A]], a: Answer[A]): Option[Correct[A]] =
        if (a.r) updateA(a.q, p)((a, b) => Some(Correct(a, b))) else p
      def updateInCorrect(p: Option[InCorrect[A]], a: Answer[A]): Option[InCorrect[A]] =
        if (!a.r) updateA(a.q, p)((a, b) => Some(InCorrect(a, b))) else p

      if (contains(a.q)) this match {
        case tc: Correct[A] => Correct(dropQ(a.q), Result(updateInCorrect(tc.result.left, a), updateCorrect(tc.result.right, a)))
        case ti: InCorrect[A] => InCorrect(dropQ(a.q), Result(updateInCorrect(ti.result.left, a), updateCorrect(ti.result.right, a)))
      }
      else this

    }

    def isHead(q: A): Boolean = qs.head == q
    def contains(q: A): Boolean = qs.contains(q)

    /**
     * Type setting needed to convince the compiler that it's a A stream
     */
    private val z: Stream[A] = Stream.Empty

    private def someQs(p: Option[Pool[A]]): Stream[A] = p match {
      case Some(p) => p.qs
      case _ => Stream.Empty
    }

    def levels: Stream[A] = fold(someQs)(_.append(_))

    def next: Stream[A] = folder(Stream(this))(z)((p: Pool[A], s: Stream[A]) => s.append(p.qs))(x => !x.isEmpty)

    //    def next = traverse(x => !x.isEmpty)

    def find(q: A): Pool[A] = finder(Stream(this))((p: Pool[A]) => p)(_.contains(q))

    def diff(p1: Pool[A], p2: Pool[A]) = ???
    def corrects: Int = ???
    def incorrects: Int = ???

    /**
     * Map
     */
    def map[B](f: Stream[A] => Stream[B]): Option[Pool[B]] = {

      def mapI[A, B](p: Option[InCorrect[A]])(f: Stream[A] => Stream[B]): Option[InCorrect[B]] = p map
        { case InCorrect(v, Result(i, c)) => InCorrect(f(v), Result(mapI(i)(f), mapC(c)(f))) }

      def mapC[A, B](p: Option[Correct[A]])(f: Stream[A] => Stream[B]): Option[Correct[B]] = p map {
        case Correct(v, Result(i, c)) => Correct(f(v), Result(mapI(i)(f), mapC(c)(f)))
      }

      this match {
        case ti: InCorrect[A] => mapI(Some(ti))(f)
        case tc: Correct[A] => mapC(Some(tc))(f)
      }
    }

    /**
     * FilterMap
     */

//    def filterMap[B,C](b:B)(f: (Pool[A],B) => Pool[C])(g:Pool[A] => Boolean): Option[Pool[C]] = {
//
//      def mapI[B, C](p: Option[InCorrect[A]])(f: (Pool[A],B) => Pool[C]): Option[InCorrect[B]] = p map
//        { case InCorrect(v, Result(i, c)) => if(g(p)) incorrect(f(v), Result(mapI(i)(f), mapC(c)(f))) }
//
//      def mapC[B, C](p: Option[Correct[A]])(f: (Pool[A],B) => Pool[C]): Option[Correct[B]] = p map {
//        case Correct(v, Result(i, c)) => Correct(f(v), Result(mapI(i)(f), mapC(c)(f)))
//      }
//
//      this match {
//        case ti: InCorrect[A] => mapI(Some(ti))(f)
//        case tc: Correct[A] => mapC(Some(tc))(f)
//      }
//    }

    //    def filterMapI[A](p: Option[Pool[A]])(f: Option[Pool[A]] => Option[Pool[A]])(g: Option[Pool[A]] => Boolean): Option[Pool[A]] = if (g(p)) filterMapI[A](f(p))(f)(g) else p

    //    def filterMap[B](a: B)(f: B => Option[Pool[A]])(g: Option[Pool[A]] => Boolean) = {
    //
    //      def filterMaps[T <: Pool[A],B](a: B, p: Option[T])(f: B => Option[T])(g: Option[T] => Boolean): Option[T] =
    //        if (g(p)) filterMaps[B](a, f(a))(f)(g) else p

    //      def filterMapI(a: B, p: Option[InCorrect[A]])(f: B => Option[InCorrect[A]])(g: Option[InCorrect[A]] => Boolean) =
    //        filterMaps[InCorrect[A],B](a, p)(f)(g)
    //
    //      def filterMapC(a: B, p: Option[Correct[A]])(f: B => Option[Correct[A]])(g: Option[Correct[A]] => Boolean) =
    //        filterMaps[Correct[A],B](a, p)(f)(g)

    //      this match {
    //        case ti: InCorrect[A] => filterMaps[InCorrect[A],B](a, Some(ti))(f)(g)
    //        case tc: Correct[A] => filterMaps[Correct[A],B](a, Some(tc))(f)(g)
    //      }
    //    }

    /**
     * Fold
     */

    def fold[B](f: Option[Pool[A]] => B)(g: (B, B) => B): B = {
      def foldL[B](t: Option[InCorrect[A]])(f: Option[Pool[A]] => B)(g: (B, B) => B): B = {
        folds[B, InCorrect[A]](t)(f)(g)
      }

      def foldR[B](t: Option[Correct[A]])(f: Option[Pool[A]] => B)(g: (B, B) => B): B = {
        folds[B, Correct[A]](t)(f)(g)
      }

      def folds[B, T <: Pool[A]](t: Option[T])(f: Option[Pool[A]] => B)(g: (B, B) => B): B = t match {
        case Some(p) =>
          p.result.flatten.toList match {
            case Nil => f(Some(p))
            case List(x: InCorrect[A], y: Correct[A]) =>
              val r = p.result
              g(foldL(r.left)(f)(g), foldR(r.right)(f)(g))
            case List(c: Correct[A]) =>
              g(f(Some(this)), foldR(Some(c))(f)(g))
            case List(i: InCorrect[A]) =>
              g(f(Some(this)), foldL(Some(i))(f)(g))
          }
        case _ => f(None)
      }

      this match {
        case i: InCorrect[A] => foldL(Some(i))(f)(g)
        case c: Correct[A] => foldR(Some(c))(f)(g)
      }
    }

    //    def foldWhile[B](f: Option[Pool[A]] => B)(g: (B, B) => B)(h: B => Boolean): B = {
    //
    //      def foldLWhile[B](t: Option[InCorrect[A]])(f: Option[Pool[A]] => B)(g: (B, B) => B)(h: B => Boolean): B = {
    //        foldsWhile[B, InCorrect[A]](t)(f)(g)(h)
    //      }
    //
    //      def foldRWhile[B](t: Option[Correct[A]])(f: Option[Pool[A]] => B)(g: (B, B) => B)(h: B => Boolean): B = {
    //        foldsWhile[B, Correct[A]](t)(f)(g)(h)
    //      }
    //
    //      def foldsWhile[B, T <: Pool[A]](t: Option[T])(f: Option[Pool[A]] => B)(g: (B, B) => B)(h: B => Boolean): B = t match {
    //        case Some(p) => if (h(f(Some(p)))) f(Some(p)) else {
    //          p.result.flatten.toList match {
    //            case Nil => f(Some(p))
    //            case List(x: InCorrect[A], y: Correct[A]) =>
    //              val r = p.result
    //              g(foldLWhile(r.left)(f)(g)(h), foldRWhile(r.right)(f)(g)(h))
    //            case List(c: Correct[A]) =>
    //              if(h(f(Some(this)))) f(Some(this)) else {
    //              g(f(Some(this)), foldRWhile(Some(c))(f)(g)(h))}
    //            case List(i: InCorrect[A]) =>
    //               if(h(f(Some(this)))) f(Some(this)) else {
    //              g(f(Some(this)), foldLWhile(Some(i))(f)(g)(h))}
    //          }
    //        }
    //        case _ => f(None)
    //      }
    //
    //      this match {
    //        case i: InCorrect[A] => foldLWhile(Some(i))(f)(g)(h)
    //        case c: Correct[A] => foldRWhile(Some(c))(f)(g)(h)
    //      }
    //    }

    def depth = this.fold(_ => 1)((d1, d2) => 1 + (d1 max d2))

    def leafs: Stream[(Stream[A], String)] = {
      type B = Stream[(Stream[A], String)]
      def counter(p: Option[Pool[A]]): B = {
        p match {
          case Some(ci) => Stream((ci.qs, ci.toString()))
          case None => Stream((Stream.Empty, ""))
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

  case class Result[A](left: Option[InCorrect[A]], right: Option[Correct[A]]) {

    def flatten: Stream[Pool[A]] = Stream(this.left, this.right).flatten
  }

  object Result {

  }

  object Pool {

    def incorrect[A](qs: Stream[A], r: Result[A]): Pool[A] = InCorrect(qs, r)
    def correct[A](qs: Stream[A], r: Result[A]): Pool[A] = Correct(qs, r)

    def folder[A, B](ps: Stream[Pool[A]])(z: B)(f: (Pool[A], B) => B)(g: B => Boolean): B = {
      if (ps.isEmpty) sys.error("ps should contain elements")
      if (g(z)) z
      else {
        val acc = ps.tail //todo list
        val h = ps.head //current element
        val hqs = f(h, z) //result stream
        h.result match {
          case Result(None, None) => //Leaf
            if (acc.isEmpty) hqs else folder(acc)(hqs)(f)(g)
          case _ => //Some node
            folder(acc.append(h.result.flatten))(hqs)(f)(g)
        }
      }
    }

    /**
     *
     */
    def finder[A, B](ps: Stream[Pool[A]])(f: (Pool[A]) => B)(g: B => Boolean): B = {
      if (ps.isEmpty) sys.error("ps should contain elements")
      else {
        val acc = ps.tail
        val h = ps.head
        val hqs = f(h)
        if (g(hqs)) hqs
        h.result match {
          case Result(None, None) => //Leaf
            if (acc.isEmpty) hqs else finder(acc)(f)(g)
          case _ => //Some node
            finder(acc.append(h.result.flatten))(f)(g)
        }
      }
    }

  }

}