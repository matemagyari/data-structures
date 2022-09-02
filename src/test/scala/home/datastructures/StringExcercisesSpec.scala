package home.datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

class StringExcercisesSpec extends AnyFlatSpec with Matchers {

  //Check Permutation: Given two strings, write a method to decide if one is a permutation of the
  //other.
  "Check if a string is a permutation of the other" should "work in O(n) time" in {
    def isPermutation(s1: String, s2: String): Boolean =
      characterOccurrances(s1) == characterOccurrances(s2)

    isPermutation("abc", "abd") shouldBe false
    isPermutation("abc", "bca") shouldBe true
  }

  // Is Unique: Implement an algorithm to determine if a string has all unique characters. What if you
  // cannot use additional data structures?
  "Contains all unique character" should "work in O(n^2) time" in {
    def allUnique(s: String): Boolean = {
      for {c1 <- s.toCharArray} {
        var count = 0
        for {c2 <- s.toCharArray if c1 == c2} {
          count = count + 1
          if (count > 1) {
            return false
          }
        }
      }
      return true
    }

    allUnique("abc") shouldBe true
    allUnique("abcbd") shouldBe false
  }

  // Palindrome Permutation: Given a string, write a function to check if it is a permutation of a palindrome.
  "Check if string is a permutation of a palindrome" should "work in O(n) time" in {

    def isPal(text: String): Boolean = {

      val occurrences: Map[Char, Int] = characterOccurrances(text)

      val evenOccurrences: Int = occurrences.values.count(_ % 2 == 0)

      // either all chars occur even times, or there is one which not
      evenOccurrences >= occurrences.size - 1
    }

    isPal("") shouldBe true
    isPal("a") shouldBe true
    isPal("ab") shouldBe false
    isPal("aa") shouldBe true
    isPal("baa") shouldBe true
    isPal("bacbac") shouldBe true
    isPal("bacbacd") shouldBe true
    isPal("bacbacde") shouldBe false
  }

  //  One Away: There are three types of edits that can be performed on strings: insert a character,
  //  remove a character, or replace a character. Given two strings, write a function to check if they are
  //    one edit (or zero edits) away.
  //  EXAMPLE
  //  pale, ple -> true
  //  pales, pale -> true
  //  pale, bale -> true
  //  pale, bake -> false

  "One away" should "work in O(n+m) time" in {

    def oneAway(text1: String, text2: String): Boolean = {

      val size1 = text1.size // O(n)
      val size2 = text2.size // O(m)
      val sizeDiff = Math.abs(size1 - size2)

      if (sizeDiff > 1)
        false
      else if (sizeDiff == 0)
        (0 to size1 - 1).count { index => text1(index) != text2(index) } <= 1 // O(n=m)
      else { // sizeDiff is 1
        val (shorter, longer) = if (text1.size > text2.size) (text2, text1) else (text1, text2)
        (0 to shorter.size - 1)
          .find { index => shorter(index) != longer(index) } // O(n)
          .map { deviatingIndex =>
            // O(n)
            val adjusted = shorter.substring(0, deviatingIndex) + longer(deviatingIndex) + shorter.substring(deviatingIndex)
            adjusted == longer
          }
          .getOrElse(true) // it means the shorter string is the same as the beginning of the longer string
      }

    }

    oneAway("pale", "ple") shouldBe true
    oneAway("ple", "pale") shouldBe true
    oneAway("pales", "pale") shouldBe true
    oneAway("pale", "bale") shouldBe true
    oneAway("pale", "bake") shouldBe false
  }

  //  String Compression: Implement a method to perform basic string compression using the counts
  //  of repeated characters. For example, the string aabcccccaaa would become a2blc5a3. If the
  //    "compressed" string would not become smaller than the original string, your method should return
  //  the original string. You can assume the string has only uppercase and lowercase letters (a - z).

  "String compression" should "work" in {

    @tailrec
    def compressHelper(currentChar: Option[Char], currentNum: Int, text: List[Char], acc: List[Char]): List[Char] = {
      def finish: List[Char] = {
        if (currentNum == 1)
          acc ++ currentChar.toList
        else
          acc ++ currentChar.toList.flatMap(c => s"$c$currentNum".toList)
      }

      text match {
        case Nil => finish
        case c +: tail =>
          if (currentChar.contains(c))
            compressHelper(currentChar, currentNum + 1, tail, acc)
          else
            compressHelper(Some(c), 1, tail, finish)
      }
    }

    def compress(text: String): String =
      compressHelper(None, 0, text.toList, List.empty).mkString


    compress("") shouldBe ""
    compress("a") shouldBe "a"
    compress("ab") shouldBe "ab"
    compress("aab") shouldBe "a2b"
    compress("aabbbcdde") shouldBe "a2b3cd2e"

  }

  //  Rotate Matrix: Given an image represented by an NxN matrix, where each pixel in the image is 4
  //  bytes, write a method to rotate the image by 90 degrees. Can you do this in place?
  "Rotate Matrix" should "work" in {

    type Matrix = Array[Array[String]]
    val n = 4
    val matrix: Matrix = new Matrix(n)

    for (row <- 1 to n) {
      matrix(row - 1) = new Array[String](n)
      for (col <- 1 to n) {
        matrix(row - 1)(col - 1) = s"r${row}c${col}"
      }
    }

    def print(m: Matrix): Unit = {
      m.foreach(row => println(row.mkString(",")))
    }

    def createDuplicate(m: Matrix): Matrix = {
      val n = m.length
      val matrix: Matrix = new Matrix(n)
      for (row <- 1 to n) {
        matrix(row - 1) = new Array[String](n)
        for (col <- 1 to n) {
          matrix(row - 1)(col - 1) = m(row - 1)(col - 1)
        }
      }
      matrix
    }

    def replaceCol(matrix: Matrix, col: Int, vals: Array[String]): Unit = {
      val n = vals.size
      val revertVals = vals.reverse
      for (index <- 0 to (n - 1)) {
        matrix(index)(col) = revertVals(index)
      }
    }

    val matrix2 = createDuplicate(matrix)
    println("Original matrix")
    print(matrix)

    for (index <- 0 to (n - 1)) {
      val row = matrix(index)
      replaceCol(matrix2, col = index, row)
    }

    println("\nRotated matrix")
    print(matrix2)

  }

  "String rotation check" should "work" in {

    // this is given
    def isSubstring(s1: String, s2: String): Boolean = s1.contains(s2)

    def isRotation(s1: String, s2: String): Boolean = s1.length == s2.length && isSubstring(s1 + s1, s2)

    isRotation("water", "water") shouldBe true

    isRotation("water", "terwa") shouldBe true
    isRotation("terwa", "water") shouldBe true

    isRotation("terw", "water") shouldBe false
    isRotation("water", "terw") shouldBe false

  }

  "find the length of the longest substring without repeating characters" should "work" in {
    def lengthOfLongestSubstring(s: String): Int = {

      def helper(startPos: Int, endPos: Int, best: Int): Int = {
        if (endPos >= s.length) best
        else {
          val nextEndPos = endPos + 1
          val repeatedCharIndex = {
            val nextChar = s(endPos)
            s.substring(startPos, endPos).indexOf(nextChar)
          }
          helper(
            startPos = startPos + repeatedCharIndex + 1,
            endPos = nextEndPos,
            best = if (repeatedCharIndex > -1) best else Math.max(best, nextEndPos - startPos))
        }
      }

      helper(startPos = 0, endPos = 0, best = 0)
    }

    lengthOfLongestSubstring("") shouldBe 0
    lengthOfLongestSubstring("a") shouldBe 1
    lengthOfLongestSubstring(" ") shouldBe 1
    lengthOfLongestSubstring("abcabcbb") shouldBe 3
    lengthOfLongestSubstring("bbbbb") shouldBe 1
    lengthOfLongestSubstring("pwwkew") shouldBe 3
    lengthOfLongestSubstring("nfpdmpi") shouldBe 5
    lengthOfLongestSubstring("bbtablud") shouldBe 6
  }

  "find the longest palindromic substring" should "work" in {
    def longestPalindrome(s: String): String = {

      def isPalindrome(str: String): Boolean = {
        val len = str.length
        (0 to len / 2).forall { index => str(index) == str(len - 1 - index) }
      }

      def findPalindrome(length: Int): Option[String] =
        (0 to s.length - length)
          .map { offset => s.substring(offset, offset + length) }
          .find(isPalindrome)

      def helper(len: Int): Option[String] =
        if (len == 0) None
        else findPalindrome(len).orElse(helper(len - 1))

      helper(s.length).getOrElse("")
    }

    longestPalindrome("") shouldBe ""
    longestPalindrome("a") shouldBe "a"
    longestPalindrome("ac") shouldBe "a"
    longestPalindrome("cabar") shouldBe "aba"
    longestPalindrome("cabaaddaa") shouldBe "aaddaa"

    val input1 = "civilwartestingwhetherthatnaptionoranynartionsoconceivedandsodedicatedcanlongendureWeareqmetonagreatbattlefiemldoftzhatwarWehavecometodedicpateaportionofthatfieldasafinalrestingplaceforthosewhoheregavetheirlivesthatthatnationmightliveItisaltogetherfangandproperthatweshoulddothisButinalargersensewecannotdedicatewecannotconsecratewecannothallowthisgroundThebravelmenlivinganddeadwhostruggledherehaveconsecrateditfaraboveourpoorponwertoaddordetractTgheworldadswfilllittlenotlenorlongrememberwhatwesayherebutitcanneverforgetwhattheydidhereItisforusthelivingrathertobededicatedheretotheulnfinishedworkwhichtheywhofoughtherehavethusfarsonoblyadvancedItisratherforustobeherededicatedtothegreattdafskremainingbeforeusthatfromthesehonoreddeadwetakeincreaseddevotiontothatcauseforwhichtheygavethelastpfullmeasureofdevotionthatweherehighlyresolvethatthesedeadshallnothavediedinvainthatthisnationunsderGodshallhaveanewbirthoffreedomandthatgovernmentofthepeoplebythepeopleforthepeopleshallnotperishfromtheearth"

    longestPalindrome(input1) shouldBe 1
  }

  "Increasing Triplet Subsequence" should "work" in {

    def sorter(a: (Int, Int), b: (Int, Int)): Boolean =
      a._1 < b._1 || (a._1 == b._1 && a._2 < b._2)

    def increasingTriplet2(nums: Array[Int]): Boolean = {
      ???

    }

    def increasingTriplet(nums: Array[Int]): Boolean = {

      def increasing(s1: Set[Int], s2: Set[Int], s3: Set[Int]) = {
        s1.exists { s1e =>
          s2.exists { s2e =>
            s2e > s1e &&
              s3.exists(_ > s2e)
          }
        }
      }

      val numsMap: Map[Int, Set[Int]] = nums
        .zipWithIndex
        .groupBy(_._1)
        .map { case (num, indices) => num -> indices.map(_._2).toSet }

      def getIndices(num: Int): Set[Int] = numsMap.get(num).getOrElse(Set.empty)

      val orderedDistinctNumbers: List[Int] = numsMap.keySet.toList.sorted

      //      println(s"orderedDistinctNumbers $orderedDistinctNumbers")

      val len = nums.length - 1

      println(s"sorted tuples ${nums.zipWithIndex.sortWith(sorter).toList}")
      println(s"orderedDistinctNumbers $orderedDistinctNumbers")

      orderedDistinctNumbers.zipWithIndex.exists { case (numI, indexI) =>

        numsMap.get(numI)

        orderedDistinctNumbers.drop(indexI + 1).zipWithIndex.exists { case (numJ, indexJ) =>
          numJ > numI && orderedDistinctNumbers.drop(indexI + indexJ + 1).exists { numK =>
            //            println(s"numI $numI numJ $numJ numK $numK ")
            numK > numJ && increasing(getIndices(numI), getIndices(numJ), getIndices(numK))
          }

        }
      }

      //      (0 to len).exists { i =>
      //        (i + 1 to len).exists { j =>
      //          nums(i) < nums(j) && distincts.exists { numK => nums(j) < numK && numsMap.get(numK).getOrElse(Set.empty).exists(_ > j) }
      //          //          (j + 1 to len).exists { k =>
      //          //            nums(j) < nums(k)
      //          //          }
      //        }
      //      }
    }

    increasingTriplet(Array[Int]()) shouldBe false
    increasingTriplet(Array(1, 2, 3, 4, 5)) shouldBe true
    increasingTriplet(Array(5, 4, 3, 2, 1)) shouldBe false
    increasingTriplet(Array(2, 1, 5, 0, 4, 6)) shouldBe true

    val input = (0 to 10).reverse.toArray
    println(s"AAA input")
    increasingTriplet(input) shouldBe false
  }

  "Group anagrams" should "work" in {

    def groupAnagrams(strs: Array[String]): List[List[String]] = {
      strs.groupBy(_.toList.sorted).map {
        case (chars, words) => words.toList
      }.toList
    }
  }

  // Given a signed 32-bit integer x, return x with its digits reversed. If reversing x causes the value to go
  //outside the signed 32-bit integer range [-231, 231 - 1], then return 0.
  "Reverse integer" should "work" in {
    def reverse(x: Int): Int = {
      val negative = x < 0
      val digits: List[Int] = {
        val chars = x.toString.toList
        (if (negative) chars.drop(1) else chars).map(_.toString.toInt)
      }
      val absReverse: Long = (0 until digits.length).map { index =>
        digits(index).toLong * Math.pow(10, index).toInt
      }.sum

      val result = (if (absReverse > Integer.MAX_VALUE) 0 else absReverse).toInt

      if (negative) -result else result
    }

    reverse(-2147483648) shouldBe 0
    reverse(1534236469) shouldBe 0
    reverse(-12) shouldBe -21
    reverse(123) shouldBe 321
    reverse(0) shouldBe 0
    reverse(10) shouldBe 1
  }


  "First unique character" should "work" in {
    def firstUniqChar(s: String): Int =
      s.toList
        .zipWithIndex
        .groupBy(_._1)
        .collect { case (_, List(occurrence)) => occurrence._2 }
        .minOption
        .getOrElse(-1)

    firstUniqChar("aabb") shouldBe -1
    firstUniqChar("loveleetcode") shouldBe 2
  }

  "Reverse string" should "work in-place with O(1) memory" in {
    def reverseString(s: Array[Char]): Unit = {

      (0 until s.length / 2).foreach { index =>
        val temp = s(index)
        s(index) = s(s.length - index - 1)
        s(s.length - index - 1) = temp
      }

    }

    val input = Array('h', 'e', 'l', 'l', 'o')
    reverseString(input)
    input shouldBe Array('o', 'l', 'l', 'e', 'h')
  }

  "Is palindrome" should "work in-place with O(1) memory" in {
    def isPalindrome(s: String): Boolean = {
      val str = s.toLowerCase.filter(_.isLetterOrDigit)
      (0 until str.length / 2).forall { index =>
        str(index) == str(str.length - index - 1)
      }
    }

    isPalindrome("A man, a plan, a canal: Panama") shouldBe true
  }

  "Is anagram" should "work" in {
    def isAnagram(s: String, t: String): Boolean = {
      def toMap(str: String) =
        str.toList.groupBy(identity).map { case (c, positions) => c -> positions.size }

      toMap(s) == toMap(t)
    }
  }

  "My atoi" should "work" in {

    def myAtoi(s: String): Int = {

      var positive = true
      val s2: List[Char] = s.dropWhile(_ == ' ').toList match {
        case Nil => List.empty
        case '-' +: rest =>
          positive = false
          rest
        case head +: rest =>
          if (head.isDigit)
            head +: rest
          else if (head == ' ' || head == '+')
            rest
          else
            List.empty
      }

      val digits: Array[Int] = s2.dropWhile(_ == '0').takeWhile(_.isDigit).toArray.map(_.toString.toInt)

      def toNumber(index: Int, acc: Int = 0): Int = {
        if (index < 0) acc
        else {
          val position = digits.size - index - 1
          val numLong = digits(index).toLong * Math.pow(10, position).toLong
          if (positive && numLong > Integer.MAX_VALUE)
            Integer.MAX_VALUE
          else if (!positive && -numLong < Integer.MIN_VALUE)
            Integer.MIN_VALUE
          else {
            val numInt = numLong.toInt
            if (positive && (Integer.MAX_VALUE - numInt < acc))
              Integer.MAX_VALUE
            else if (!positive && (Integer.MIN_VALUE + numInt > acc))
              Integer.MIN_VALUE
            else {
              val newAcc = if (positive) acc + numInt else acc - numInt
              toNumber(index - 1, newAcc)
            }
          }
        }
      }

      toNumber(index = digits.size - 1)
    }

    println(s"Integer.MIN_VALUE [${Integer.MIN_VALUE}]")
    println(s"Integer.MIN_VALUE [${Integer.MIN_VALUE + 10}]")

    myAtoi("0032") shouldBe 32
    myAtoi(" 32") shouldBe 32
    myAtoi("32") shouldBe 32
    myAtoi("32 ") shouldBe 32
    myAtoi("32abc") shouldBe 32
    myAtoi("+32") shouldBe 32
    myAtoi("-32") shouldBe -32
    myAtoi("  -32") shouldBe -32
    myAtoi("words and 987") shouldBe 0
    myAtoi("-91283472332") shouldBe -2147483648
    myAtoi("-6147483648") shouldBe -2147483648
  }

  "indexOf" should "work" in {
    def strStr(haystack: String, needle: String): Int = {

      if (needle.isEmpty)
        0
      else {
        val nLen = needle.length
        val hLen = haystack.length

        (0 to hLen - nLen)
          .find { index => haystack.substring(index, index + nLen) == needle }
          .getOrElse(-1)
      }

    }

    strStr("", "") shouldBe 0
    strStr("abc", "bc") shouldBe 1
    strStr("abc", "bcd") shouldBe -1
  }

  "longest prefix" should "work" in {
    def longestCommonPrefix(strs: Array[String]): String = {
      def helper(a: Array[String], acc: List[Char] = List.empty): List[Char] = {
        if (a.exists(_.isBlank)) acc
        else {
          val temp: Array[(Char, String)] = a.map { str => (str.head, str.tail)}
          if (temp.map(_._1).distinct.size == 1)
            helper(temp.map(_._2), acc :+ temp.head._1)
          else acc
        }
      }

      helper(strs).mkString
    }
    longestCommonPrefix(Array("flower","flow","flight")) shouldBe "fl"
  }

  private def characterOccurrances(text: String): Map[Char, Int] = text.foldLeft(Map.empty[Char, Int]) {
    case (acc, char) =>
      val num = acc.getOrElse(char, 0) + 1
      acc + (char -> num)
  }

}
