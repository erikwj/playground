package pool

import org.specs2.mutable.Specification
import LTreez._
import Tree._

object LTreezSpec extends Specification {

  val l: Tree[Int] = leaf(3)
		  val b: Tree[Int] = node(leaf(1), leaf(2), leaf(3))
		  val sb: Tree[Stream[Int]] = node(leaf(Stream(1,2,3)), node(leaf(Stream(1)), leaf(Stream(2)), leaf(Stream(3))), leaf(Stream(3)))

  "map should work" in {
    def add1(x: Int): Int = x + 1 
    def add1String(x: Stream[Int]): Stream[String] = x map { _.toString + "_1" }
    Tree.map(b)(add1) must_== node(leaf(2),leaf(3),leaf(4))
    
     def add1S(x: Stream[Int]): Stream[Int] = x map { _ + 1 }
    Tree.map(sb)(add1S) must_== node(leaf(Stream(2,3,4)), node(leaf(Stream(2)), leaf(Stream(3)), leaf(Stream(4))), leaf(Stream(4)))
   
  }
  
}