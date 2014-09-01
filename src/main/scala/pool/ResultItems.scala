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

    private val z: Stream[A] = Stream.Empty
    def levels: Stream[A] = folder(Stream(this))(z)((p: Pool[A], s: Stream[A]) => s.append(p.qs))(_ => false)
    def next: Stream[A] = folder(Stream(this))(z)((p: Pool[A], s: Stream[A]) => s.append(p.qs))(x => !x.isEmpty)

    def find(q: A): Pool[A] = finder(Stream(this))((p: Pool[A]) => p)(_.contains(q))

    def diff(p1: Pool[A], p2: Pool[A]) = ???
    def corrects: Int = ???
    def incorrects: Int = ???

    /**
     * Map
     */
    def map[B](f: Stream[A] => Stream[B]): Option[Pool[B]] = {

      def mapI[A, B](p: Option[InCorrect[A]])(f: Stream[A] => Stream[B]): Option[InCorrect[B]] = p map {
        _ match { case InCorrect(v, Result(i, c)) => InCorrect(f(v), Result(mapI(i)(f), mapC(c)(f))) }
      }

      def mapC[A, B](p: Option[Correct[A]])(f: Stream[A] => Stream[B]): Option[Correct[B]] = p map {
        _ match { case Correct(v, Result(i, c)) => Correct(f(v), Result(mapI(i)(f), mapC(c)(f))) }
      }

      this match {
        case ti: InCorrect[A] => mapI(Some(InCorrect(ti.qs, ti.result)))(f)
        case tc: Correct[A] => mapC(Some(Correct(tc.qs, tc.result)))(f)
      }
    }

    def depth = fold(Some(this))(_ => 1)((d1, d2) => 1 + (d1 max d2))

  }

  case class InCorrect[A](qs: Stream[A], result: Result[A]) extends Pool[A]

  case class Correct[A](qs: Stream[A], result: Result[A]) extends Pool[A]

  case class Result[A](left: Option[InCorrect[A]], right: Option[Correct[A]]) {

    def flatten: Stream[Pool[A]] = Stream(this.left, this.right).flatten
  }

  object Result {

  }

  object Pool {

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

    def fold[A, B](t: Option[Pool[A]])(f: Option[Stream[A]] => B)(g: (B, B) => B): B = t match {
      case Some(p) => p.result.flatten match {

        case Stream.Empty => f(Some(p.qs))
        case Stream(x: InCorrect[A], y: Correct[A]) =>
          val r = p.result
          g(foldL(r.left)(f)(g), foldR(r.right)(f)(g))
        case Stream(c: Correct[A]) => foldR(Some(c))(f)(g)
        case Stream(i: InCorrect[A]) => foldL(Some(i))(f)(g)
      }
      case _ => f(None)
    }

    def foldL[A, B](t: Option[InCorrect[A]])(f: Option[Stream[A]] => B)(g: (B, B) => B): B = folds[A, B, InCorrect[A]](t)(f)(g)

    def foldR[A, B](t: Option[Correct[A]])(f: Option[Stream[A]] => B)(g: (B, B) => B): B = folds[A, B, Correct[A]](t)(f)(g)

    def folds[A, B, T <: Pool[A]](t: Option[T])(f: Option[Stream[A]] => B)(g: (B, B) => B): B = t match {
      case Some(p) => p.result match {

        case Result(None, None) => f(Some(p.qs))
        case _ =>
          val r = p.result
          g(foldL(r.left)(f)(g), foldR(r.right)(f)(g))
      }
      case _ => f(None)
    }

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

    def traverse[A, B](p: Option[Pool[A]])(f: (Pool[A]) => B)(g: B => Boolean): Option[B] = {
      p match {
        case Some(h) =>
          h.result match {
            case Result(None, None) => //Leaf
              if (g(f(h))) Some(f(h)) else None
            case _ => //Some branch
              traverse(h.result.left)(f)(g)
              traverse(h.result.right)(f)(g)
          }
        case _ => None
      }
    }

  }

}