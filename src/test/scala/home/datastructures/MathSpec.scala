package home.datastructures

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class MathSpec extends AnyFlatSpec with Matchers with OptionValues {

  "Happy number" should "work" in {
    def isHappy(n: Int): Boolean = {

      val alreadyTried: ListBuffer[Int] = ListBuffer.empty

      def next(n: Int): Int =
        n.toString.toArray
          .map(_.toString.toInt)
          .map(x => x * x)
          .sum

      @tailrec
      def helper(n: Int): Boolean = {
        if (n == 1) true
        else if (alreadyTried.contains(n)) false
        else {
          alreadyTried += n
          helper(next(n))
        }
      }

      helper(n)

    }

    isHappy(19) shouldBe true
    isHappy(2) shouldBe false
  }

 "Sqrt" should "work" in {
   def mySqrt(x: Int): Int = {

     @tailrec
     def helper(start: Int, end: Int): Int = {
//       println(s"x $x start $start end $end")
       val middle = start + (end - start) / 2
       val xm = x / middle
       if (middle == xm || middle == start) middle
       else if (xm < middle) helper(start, middle)
       else helper(middle, end)
     }

     if (x == 0) 0 else helper(1, x)
   }

   mySqrt(0) shouldBe 0
   mySqrt(1) shouldBe 1
   mySqrt(9) shouldBe 3
   mySqrt(8) shouldBe 2
   mySqrt(2147395599) shouldBe 46339
  }



}

