package home.datastructures

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

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


  "Pow" should "work" in {

    def myPow(x: Double, n: Int): Double = {

      println(s"x $x n $n")

      @scala.annotation.tailrec
      def helper(acc: Double, m: Int): Double = {
        if (m == 1) acc
        else helper(acc * x, m - 1)
      }

      if (x == 1) 1
      else if (n == 0) 1
      else if (x == -1)
        if (n % 2 == 0) 1 else -1
      else if (n == Integer.MIN_VALUE) 0
      else if (n < 0) 1 / myPow(x, -n)
      else helper(x, n)
    }

    def trunc(d: Double) = BigDecimal(d).setScale(5, RoundingMode.HALF_UP).toDouble

    def myPowTrunced(x: Double, n: Int): Double = trunc(myPow(x, n))

    myPowTrunced(2.00000, 10) shouldBe 1024.00000
    myPowTrunced(2.10000, 3) shouldBe 9.26100
    myPowTrunced(2.00000, -2) shouldBe 0.25000
    myPowTrunced(2.00000, -2147483648) shouldBe 0
    myPowTrunced(-1.00000, 2147483647) shouldBe -1.0000
  }

  "Factorial Trailing zeros" should "work" in {

    def trailingZeroes(n: Int): Int = {
      @scala.annotation.tailrec
      def helper(pow: Int, acc: Int): Int = {
        val result: Int = n / Math.pow(5, pow).toInt
        if (result == 0) acc
        else helper(pow + 1, acc + result)
      }

      helper(1, 0)
    }

    trailingZeroes(4) shouldBe 0
    trailingZeroes(6) shouldBe 1
    trailingZeroes(10) shouldBe 2
  }

  // without using multiplication, division, and mod operator
  "Divide 2 integres" should "work" in {

    def divide(dividend: Int, divisor: Int): Int = {

      val nonNegative: Boolean = dividend >= 0

      def reverse(i: Int): Int = i match {
        case Integer.MIN_VALUE => Integer.MAX_VALUE
        case Integer.MAX_VALUE => Integer.MIN_VALUE
        case _ => -i
      }

      @scala.annotation.tailrec
      def helper(result: Int, count: Int): Int = {
        if (result == dividend - divisor) count + 1
        else if (nonNegative && result > dividend - divisor) count
        else if (!nonNegative && result < dividend - divisor) count
        else helper(result + divisor, count + 1)
      }

      if (divisor == 1) dividend
      else if (divisor == -1) reverse(dividend)
      else if (divisor == Integer.MIN_VALUE && dividend != divisor) 0
      else (dividend >= 0, divisor >= 0) match {
        case (true, false) =>
          reverse(divide(dividend, reverse(divisor)))
        case (false, true) =>
          reverse(divide(dividend, reverse(divisor)))
        case _ => helper(0, 0)
      }
    }

    divide(-7, 3) shouldBe -2

    divide(-2147483648, -1) shouldBe 2147483647

    divide(-1, -1) shouldBe 1
    divide(10, 3) shouldBe 3
    divide(10, 4) shouldBe 2
    divide(10, 5) shouldBe 2
    divide(10, 6) shouldBe 1
    divide(7, -3) shouldBe -2
    divide(-7, 3) shouldBe -2
    divide(-2147483648, 1) shouldBe -2147483648

    divide(2147483647, 2) shouldBe 1073741823
    divide(-1006986286, -2145851451) shouldBe 0
    divide(-1010369383, -2147483648) shouldBe 0
    divide(1038925803, -2147483648) shouldBe 0
    divide(2147483647, -2147483648) shouldBe 0
    divide(-2147483648, -122481295) shouldBe 17
    divide(-2147483648, 2) shouldBe -1073741824
  }

  "Fraction to Recurring Decimal" should "work" in {
//    def fractionToDecimal(numerator: Int, denominator: Int): String = {
//      val isFractional: Boolean = numerator % denominator != 0
//    }



    def fractionToDecimal(numerator: Int, denominator: Int): String = {

      val isFractional: Boolean = numerator % denominator != 0

      def toDigits(i: Int): List[Int] = i.toString.toList.map(_.toString.toInt)

      def helper2(
                 currentNumerator: Int,
                  acc: List[Int] = List.empty,
                  remainders: Set[Int] = Set.empty): String = {

        val remainder = currentNumerator % denominator
        val quotient = currentNumerator / denominator

        val newAcc = acc ++ toDigits(quotient)

        quotient.toString

      }

      def findSmallest(currentNumerator: List[Int]): Option[List[Int]] = {
        if (currentNumerator.headOption.exists(_ == 0))
          Some(List(0))
        else
          (1 to currentNumerator.size)
            .map(currentNumerator.take)
            .find(_.mkString.toInt > denominator)
      }

      def toNum(acc: List[Int]): String =
        if (isFractional) {
          val sacc = acc.map(_.toString)
          (sacc.headOption.toList ++ List(".") ++ sacc.drop(1)).mkString
        }
        else
          acc.mkString

      def helper(currentNumerator: List[Int], acc: List[Int] = List.empty, numerators: List[List[Int]] = List.empty): String = {
        //        println(s"currentNumerator $currentNumerator")
        //        println(s"numerators $numerators")
        //        println(s"acc ${acc.mkString}")

        if (currentNumerator.isEmpty)
          toNum(acc)
        else if (currentNumerator.forall(_ == 0))
          toNum(acc ++ currentNumerator.drop(1))
        else
          (0 until numerators.size)
            .find { idx => numerators(idx) == currentNumerator }
            .map { precedentIdx =>
              val len = numerators.size - precedentIdx
              val repeated = acc.takeRight(len).mkString
              s"${toNum(acc).dropRight(len)}($repeated)"
            }
            .getOrElse {
              val newNumerators = numerators :+ currentNumerator
              findSmallest(currentNumerator)
                .map { smallest =>
                  val n = smallest.mkString.toInt
                  val remainder = n % denominator
                  val quotient = n / denominator

                  val newAcc = acc :+ quotient

                  val newDigitInNumerator: Option[Int] =
                    currentNumerator.drop(smallest.size).headOption

                  val newCurrentNumerator =
                    if (smallest == currentNumerator && remainder != 0)
                      remainder.toString.toList.map(_.toString.toInt)
                    else
                      remainder.toString.toList.map(_.toString.toInt) :+ 0

                  println(s"num: ${currentNumerator.mkString} quotient $quotient remainder $remainder newAcc ${newAcc.mkString}")

                  if (remainder == 0 && smallest == currentNumerator)
                    toNum(newAcc)
                  else
                    helper(newCurrentNumerator, newAcc,
                      numerators = newNumerators)
                }
                .getOrElse {
                  helper(currentNumerator :+ 0, acc :+ 0, newNumerators)
                }

            }
      }


      if (numerator == 0) "0" else
        helper(numerator.toString.toList.map(_.toString.toInt))
    }

//        fractionToDecimal(2, 1) shouldBe "2"
//        fractionToDecimal(1, 2) shouldBe "0.5"
//        fractionToDecimal(1, 25) shouldBe "0.04"
//        fractionToDecimal(0, 3) shouldBe "0"
//        fractionToDecimal(500, 10) shouldBe "50"
    //        fractionToDecimal(4, 333) shouldBe "0.(012)"
    fractionToDecimal(22, 7) shouldBe "3.(142857)"
  }


}

