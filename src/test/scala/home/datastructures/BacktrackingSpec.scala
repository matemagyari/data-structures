package home.datastructures

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.{List, _}

class BacktrackingSpec extends AnyFlatSpec with Matchers with OptionValues {

  "Letter Combinations of a Phone Number" should "work" in {
    def letterCombinations(digits: String): List[String] = {

      val letters: Map[Int, Set[Char]] = Map(
        2 -> "abc",
        3 -> "def",
        4 -> "ghi",
        5 -> "jkl",
        6 -> "mno",
        7 -> "pqrs",
        8 -> "tuv",
        9 -> "wxyz"
      ).map { case (k, v) => k -> v.toSet }

      val intDigits: List[Int] = digits.map(_.toString.toInt).toList

      def helper(remainingLetters: List[Int], acc: Set[String]): Set[String] =
        remainingLetters match {
          case Nil => acc
          case head :: tail =>
            val newAcc = {
              if (acc.isEmpty)
                letters(head).map(_.toString)
              else
                letters(head).flatMap { c =>
                  acc.map { s => s + c }
                }
            }
            helper(tail, newAcc)
        }

      helper(intDigits, Set.empty).toList

    }

    letterCombinations("23").toSet shouldBe Set("ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf")
  }

  "Generate Parentheses" should "work" in {
    def generateParenthesis(n: Int): List[String] = {

      def backtrack(candidates: List[String], solutions: List[String]): List[String] = {
        candidates match {
          case Nil => solutions
          case candidate :: rest =>
            if (candidate.count(_ == '(') > n)
              backtrack(rest, solutions)
            else if (candidate.count(_ == '(') < candidate.count(_ == ')'))
              backtrack(rest, solutions)
            else if (candidate.length == n * 2)
              backtrack(rest, candidate +: solutions)
            else {
              val children = List(s"$candidate(", s"$candidate)")
              backtrack(children ++ rest, solutions)
            }
        }
      }

      backtrack(List(""), List.empty)
    }

    generateParenthesis(1) shouldBe List("()")
    generateParenthesis(2).toSet shouldBe Set("(())", "()()")
    generateParenthesis(3).toSet shouldBe Set("((()))", "(()())", "(())()", "()(())", "()()()")
  }


  "Word search" should "work" in {
    def exist(board: Array[Array[Char]], word: String): Boolean = {
      val maxY = board.length - 1
      val maxX = board(0).length - 1
      case class Position(x: Int, y: Int)
      def letter(position: Position): Char = board(position.y)(position.x)

      def toWord(positions: List[Position]): String = positions.map(letter).mkString

      def neighbours(position: Position): Set[Position] =
        for {
          newX <- (position.x - 1 to position.x + 1).toSet
          if newX <= maxX && newX >= 0
          newY <- (position.y - 1 to  position.y + 1).toSet
          if newY <= maxY && newY >= 0
          if (Math.abs(newX - position.x) + Math.abs(newY - position.y)) == 1
        } yield Position(newX, newY)

      def backtrack(candidates: List[List[Position]]): Boolean = {
        candidates match {
          case Nil => false
          case candidate :: rest =>
            val wordCandidate = toWord(candidate)
            println(s"wordCandidate [$candidate]")
            println(s"wordCandidate [$wordCandidate]")
            if (word.take(candidate.size) != wordCandidate)
              backtrack(rest)
            else if (wordCandidate == word)
            //backtrack(rest, candidate +: solutions)
              true
            else {
              val children = candidate
                .lastOption
                .toSet
                .flatMap(neighbours)
                .filterNot(candidate.contains)
                .map(next => candidate :+ next)
                .toList

              backtrack(children ++ rest)
            }
        }
      }

      val startingPoints: Seq[Position] = for {
        x <- 0 to maxX
        y <- 0 to maxY
      } yield Position(x, y)

      backtrack(startingPoints.toList.map(p => List(p)))
    }

    exist(
      board = Array(
        Array('A', 'B', 'C', 'E'),
        Array('S', 'F', 'C', 'S'),
        Array('A', 'D', 'E', 'E')),
      word = "SEE") shouldBe true

    exist(
      board = Array(
        Array('A', 'B'),
        Array('S', 'F')),
      word = "BF") shouldBe true

    exist(
      board = Array(
        Array('A','B','C','E'),
        Array('S','F','C','S'),
        Array('A','D','E','E')),
      word = "ABCB") shouldBe false

  }

  "Permutations" should "work" in {
    def permute(nums: Array[Int]): List[List[Int]] =
      if (nums.length == 1) List(nums.toList)
      else
        nums.toList.flatMap { num =>
          val rest = nums.filterNot(_ == num)
          permute(rest).map { perm => num +: perm }
        }

    permute(Array(1, 2, 3)) shouldBe 1
  }

  "Power set" should "work" in {
    //      def backtrack(len: Int, numbers: List[Int], current: List[Int], solutions: List[List[Int]]): List[List[Int]] = {
    //        if (current.size == len) current +: solutions
    //        else {
    //          numbers.flatMap { num =>
    //            val rest = numbers.filterNot(_ == num)
    //            backtrack(len - 1, rest, num +: current, solutions)
    //          }
    //        }
    //      }
    //
    //      (0 until nums.size).toList.flatMap { len => backtrack(len, nums.toList, List.empty, List.empty) }

    def subsets(nums: Array[Int]): List[List[Int]] = {

      nums.toList match {
        case Nil => List(List.empty)
        case head :: tail =>
          val oneSmaller = subsets(tail.toArray)
          oneSmaller.map(_ :+ head) ++ oneSmaller
      }

    }

    subsets(Array(1, 2)) shouldBe List(List(2, 1), List(1), List(2), List())
    subsets(Array(1, 2, 3)) shouldBe List(List(2, 1), List(1), List(2), List())
  }
}
