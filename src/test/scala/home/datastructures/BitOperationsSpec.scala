package home.datastructures

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BitOperationsSpec extends AnyFlatSpec with Matchers with OptionValues {

  "Basics" should "work" in {

    // n & 1 - to check if a number even or not
    3 & 1 shouldBe 1
    4 & 1 shouldBe 0

    //    A single left shift multiplies a number by 2:
    //    0010 << 1  →  0100
    //    0010 << 2  →  1000
    2 << 1 shouldBe 4
    4 << 1 shouldBe 8
    7 << 1 shouldBe 14

    // shift left by 2 should multiply by 4
    2 << 2 shouldBe 8
    4 << 2 shouldBe 16
    // 101 -> 10100
    5 << 2 shouldBe 20

    //    When shifting right with a logical right shift, the least-significant bit is lost and a 00 is inserted on the other end.
    2 >> 1 shouldBe 1
    4 >> 1 shouldBe 2
    5 >> 1 shouldBe 2 // last bit falls off

    // XOR
    //101 ^ 110 = 011
    5 ^ 6 shouldBe 3

  }

  "Convert to binary" should "work" in {

    def convert(num: Int): String = {

      def helper(n: Int, acc: List[Int]): List[Int] =
        if (n == 0) acc
        else helper(n >> 1, (n & 1) +: acc)

      helper(num, List.empty).mkString
    }

    convert(1) shouldBe "1"
    convert(2) shouldBe "10"
    convert(3) shouldBe "11"
    convert(4) shouldBe "100"
    convert(5) shouldBe "101"
  }

  "Count set bits" should "work" in {

    def countBits(n: Int): Int = {

      // push the bitstring right and count the 1-s at the end, until nothing left
      def countBitsAux(num: Int, count: Int = 0): Int =
        if (num == 0) count
        else countBitsAux(num >> 1, count + (num & 1))

      countBitsAux(n)
    }

    countBits(1) shouldBe 1
    countBits(2) shouldBe 1
    countBits(3) shouldBe 2
    countBits(4) shouldBe 1
    countBits(5) shouldBe 2
    countBits(6) shouldBe 2
    countBits(7) shouldBe 3
  }

  "Hamming distance" should "work" in {
    def hammingDistance(x: Int, y: Int): Int = {

      def countBits(num: Int, count: Int = 0): Int =
        if (num == 0) count
        else countBits(num >> 1, count + (num & 1))

      countBits(x ^ y)
    }
  }

  "Hamming weight" should "work" in {

    def hammingWeight(n: Int): Int = {

      // push the bitstring right and count the 1-s at the end, until nothing left
      def countBitsAux(num: Int, count: Int = 0): Int =
        if (num == 0) count
        else countBitsAux(num >> 1, count + (num & 1))

      if (n >= 0)
        countBitsAux(n)
      else
        countBitsAux(Math.abs(Integer.MIN_VALUE - n)) + 1
    }

    hammingWeight(1) shouldBe 1
    hammingWeight(2) shouldBe 1
    hammingWeight(3) shouldBe 2
    hammingWeight(4) shouldBe 1
    hammingWeight(5) shouldBe 2
    hammingWeight(6) shouldBe 2
    hammingWeight(7) shouldBe 3
    hammingWeight(-3) shouldBe 31
  }

  "Reverse bits" should "work" in {
    // you need treat n as an unsigned value


    def reverseBits(x: Int): Int = {

      def helper(pow: Int = 0, sum: Int = 0): Int =
        if (pow == 31) sum
        else {

          val updatedSum = {
            val bitInPlace: Boolean = (x & (1 << pow)) > 0
            if (bitInPlace) sum + (1 << (31 - pow)) else sum
          }
          helper(pow + 1, sum = updatedSum)
        }

      helper() + (if (x < 0) 1 else 0)
    }

    //    00000010100101000001111010011100
    // -> 00111001011110000010100101000000
    reverseBits(43261596) shouldBe 964176192
  }

  "Pascal's triangle" should "work" in {

    def nextLine(line: List[Int]): List[Int] = {
      line.headOption.toList ++
        (1 until line.length).toList.map { idx =>
          line(idx - 1) + line(idx)
        } ++ line.lastOption.toList
    }

    def generate(numRows: Int): List[List[Int]] = {

      def generator(lists: List[List[Int]], counter: Int): List[List[Int]] =
        if (counter == 0) lists
        else generator(lists ++ lists.lastOption.map(nextLine).toList, counter - 1)

      generator(List(List(1)), counter = numRows - 1)
    }

    nextLine(List(1)) shouldBe List(1, 1)
    nextLine(List(1, 3, 3, 1)) shouldBe List(1, 4, 6, 4, 1)

    generate(3) shouldBe List(List(1), List(1, 1), List(1, 2, 1))
  }

  "Valid Parentheses" should "work" in {

    def isValid(s: String): Boolean = {

      val pairs = Map(
        '(' -> ')',
        '{' -> '}',
        '[' -> ']'
      )

      def helper(str: List[Char], stack: List[Char]): Boolean = str match {
        case Nil => stack.isEmpty
        case head :: rest =>
          if (pairs.keySet.contains(head))
            helper(rest, head +: stack)
          else {
            val expectedOpen = pairs.collect { case (open, close) if close == head => open }.head
            stack.headOption.exists(_ == expectedOpen) && helper(rest, stack.drop(1))
          }
      }

      helper(s.toList, List.empty[Char])
    }

    isValid("") shouldBe true
    isValid("()") shouldBe true
    isValid("{[]}") shouldBe true
    isValid("([)]") shouldBe false
  }

  "Finding missing number" should "work" in {
    def missingNumber2(nums: Array[Int]): Int = {

      val temp = new Array[Boolean](nums.length + 1)
      nums.foreach { n =>
        temp(n) = true
      }

      temp.zipWithIndex.collectFirst { case (false, n) => n }.get
    }

    def missingNumber(nums: Array[Int]): Int = {
      val n = nums.size
      n * (n + 1) / 2 - nums.sum
    }

    missingNumber(Array(3,0,1)) shouldBe 2
  }

}
