package pool

object ParamItems {

  case class Answer[A](q: A, r: Boolean)

  sealed abstract class Pool[A] {
    import Pool._

    def qs: Stream[A]
    def c: Option[Correct[A]]
    def i: Option[InCorrect[A]]

    def stripQuestion(q: A): Stream[A] = qs.filter(_ != q)

    def appendQ(q: A) = qs.append(Stream(q))

    private def updateNode[T <: Pool[A]](ic: Option[T], q: A)(f: (Stream[A], Option[InCorrect[A]], Option[Correct[A]]) => T): T = ic match {
      case Some(c) => f(c.appendQ(q), c.i, c.c)
      case _ => f(Stream(q), None, None)
    }

    private def updateCorrect(c: Option[Correct[A]], a: Answer[A]): Option[Correct[A]] = if (a.r) Some(updateNode[Correct[A]](c, a.q)(Correct(_, _, _))) else c
    private def updateInCorrect(c: Option[InCorrect[A]], a: Answer[A]): Option[InCorrect[A]] = if (!a.r) Some(updateNode[InCorrect[A]](c, a.q)(InCorrect(_, _, _))) else c

    def update(a: Answer[A]): Pool[A] = this match {
      case tc: Correct[A] => Correct(stripQuestion(a.q), updateInCorrect(tc.i, a), updateCorrect(tc.c, a))
      case ti: InCorrect[A] => InCorrect(stripQuestion(a.q), updateInCorrect(ti.i, a), updateCorrect(ti.c, a))
    }

    def next: A = ???
    def find = ???
    def depth = ???
    def corrects: Int = ???
    def incorrects: Int = ???
    

    def isHead(q: A): Boolean = qs.head == q
    def contains(q: A): Boolean = qs.contains(q)

    def levels: Stream[A] = {
      def go(ps: Stream[Pool[A]], res: Stream[A]): Stream[A] = {
        if (ps.isEmpty) sys.error("ps should contain elements") 
        else {
          val acc = ps.tail
          ps.head match {
            case InCorrect(q, Some(i), Some(c)) =>
              go(acc.append(Stream(i, c)), res.append(q))
            case InCorrect(q, Some(i), None) =>
              go(acc.append(Stream(i)), res.append(q))
            case InCorrect(q, None, Some(c)) =>
              go(acc.append(Stream(c)), res.append(q))
            case Correct(q, Some(i), Some(c)) =>
              go(acc.append(Stream(i, c)), res.append(q))
            case Correct(q, Some(i), None) =>
              go(acc.append(Stream(i)), res.append(q))
            case Correct(q, None, Some(c)) =>
              go(acc.append(Stream(c)), res.append(q))
            case InCorrect(q, None, None) =>
              if (acc.isEmpty) res.append(q) else go(acc, res.append(q))
            case Correct(q, None, None) =>
              if (acc.isEmpty) res.append(q) else go(acc, res.append(q))
          }
        }
      }
      go(Stream(this), Stream.Empty)
    }
  }

  //   def traverse(p: Pool[A], acc: Stream[Pool[A]], res: Stream[A]): Stream[A] = {
  //      def go(ps: Stream[Pool[A]], res: Stream[A]) = traverse(ps.head, ps.tail, res)
  //
  //      this match {
  //
  //        case InCorrect(q, Some(i), Some(c)) =>
  //          go(acc.append(Stream(i, c)), res.append(q))
  //
  //        case InCorrect(q, Some(i), None) =>
  //          go(acc.append(Stream(i)), res.append(q))
  //
  //        case InCorrect(q, None, Some(c)) =>
  //          go(acc.append(Stream(c)), res.append(q))
  //
  //        case Correct(q, Some(i), Some(c)) =>
  //          go(acc.append(Stream(i, c)), res.append(q))
  //
  //        case Correct(q, Some(i), None) =>
  //          go(acc.append(Stream(i)), res.append(q))
  //
  //        case Correct(q, None, Some(c)) =>
  //          go(acc.append(Stream(c)), res.append(q))
  //
  //        case InCorrect(q, None, None) => if (acc.isEmpty) res.append(q) else
  //          acc match {
  //            case x #:: y => traverse(x, y, res.append(q))
  //            case _ => traverse(acc.head, Stream.Empty, res.append(q))
  //          }
  //        case Correct(q, None, None) => if (acc.isEmpty) res.append(q) else
  //          acc match {
  //            case x #:: y => traverse(x, y, res.append(q))
  //            case _ => traverse(acc.head, Stream.Empty, res.append(q))
  //          }
  //      }
  //
  //    }

  case class InCorrect[A](qs: Stream[A], i: Option[InCorrect[A]], c: Option[Correct[A]]) extends Pool[A]

  case class Correct[A](qs: Stream[A], i: Option[InCorrect[A]], c: Option[Correct[A]]) extends Pool[A]
  
  case class Result[A](left:Option[InCorrect[A]],right:Option[Correct[A]])

  object Pool {

    //        def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    //      case Leaf(v) => Leaf(f(v))
    //      case Branch(l,r) => Branch(map(l)(f),map(r)(f))
    //    }
    //
    //    def fold[A,B](t: Tree[A])(f: A => B)(g:(B,B) => B): B = t match {
    //      case Leaf(v) => f(v)
    //      case Branch(l,r) => g(fold(l)(f)(g),fold(r)(f)(g))
    //
    //    }

  }

  //State 0
  val s0 = InCorrect(Stream(1, 2, 3, 4, 5, 6), None, None)
  val a1: Answer[Int] = Answer(1, false)
  val a2: Answer[Int] = Answer(2, true)
  //State 1 => updated State 0 by Answer
  val sa = s0.update(a1)
  //  val p = Pool.apply(sa._1, sa._2, sa._3)

  //  Pool.create(s0, qs, i, )
  val s1 = InCorrect(Stream(2, 3, 4, 5, 6), Some(InCorrect(Stream(1), None, None)), None)
  val s2 = InCorrect(Stream(3, 4, 5, 6), Some(InCorrect(Stream(1), None, None)), Some(Correct(Stream(2), None, None)))
  val s3 = InCorrect(Stream(4, 5, 6), Some(InCorrect(Stream(1, 3), None, None)), Some(Correct(Stream(2), None, None)))
  val s6 = InCorrect(Stream(), Some(InCorrect(Stream(1, 3), None, None)), Some(Correct(Stream(2, 4, 5, 6), None, None)))
  val s7 = InCorrect(Stream(), Some(InCorrect(Stream(3), Some(InCorrect(Stream(1), None, None)), None)), Some(Correct(Stream(2, 4, 5, 6), None, None)))

}