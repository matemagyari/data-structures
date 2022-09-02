package home.datastructures

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DynamicProgrammingSpec extends AnyFlatSpec with Matchers with OptionValues {

  "Climb stairs" should "work" in {
    def climbStairs(n: Int): Int = {
      if (n == 0) 0
      else if (n == 1) 1
      else if (n == 2) 2 // 1x2 or 2x1
      else climbStairs(n - 2) + climbStairs(n - 1)
    }
  }

  "Best time to buy and sell stocks" should "work" in {
    def maxProfit(prices: Array[Int]): Int = {

      var minPrice = Integer.MAX_VALUE
      var maxPrice = 0

      (0 to prices.length - 1).foreach { idx =>
        val price = prices(idx)
        if (price < minPrice) {
          minPrice = price
        } else if (maxPrice < price - minPrice) {
          maxPrice = price - minPrice
        }
      }
      maxPrice

    }

    maxProfit(Array(7, 1, 5, 3, 6, 4)) shouldBe 5
  }

  "Rob houses" should "work" in {


    def rob(nums: Array[Int]): Int = {

      // TOP-BOTTOM: works, but slow
      def max(n: Int): Int = n match {
        case -1 => 0
        case 0 => nums(0)
        case 1 => Math.max(nums(0), nums(1))
        case _ =>
          // either we pick this house (so we can't pick n-1)
          val choice1 = max(n - 2) + nums(n)
          // or we don't pick one, so we can pick n-1
          val choice2 = max(n - 1)
          Math.max(choice1, choice2)
      }
      //      max(nums.length-1)

      // BOTTOM-TOP

      nums.length match {
        case 0 => 0
        case 1 => nums(0)
        case 2 => Math.max(nums(0), nums(1))
        case _ =>

          var maxNMin2 = nums(0)
          var maxNMin1 = Math.max(nums(0), nums(1))

          (2 to nums.length - 1).foreach { n =>
            val latest = Math.max(maxNMin1, maxNMin2 + nums(n))

            maxNMin2 = maxNMin1
            maxNMin1 = latest
          }

          maxNMin1
      }

    }

    rob(Array.empty[Int]) shouldBe 0
    rob(Array(2)) shouldBe 2
    rob(Array(1, 2, 3)) shouldBe 4
    rob(Array(2, 7, 9, 3, 1)) shouldBe 12
  }

  "Random shuffle of an array" should "work" in {


    class Solution(_nums: Array[Int]) {

      private val original: Array[Int] = copyArray(_nums)

      def reset(): Array[Int] = {
        copyArray(original, _nums)
        _nums
      }

      def shuffle(): Array[Int] = {
        val list = scala.collection.mutable.ListBuffer.empty[Int]
        _nums.foreach { e =>
          list.append(e)
        }

        (0 to original.length - 1).foreach { i =>
          val idx = scala.util.Random.nextInt(list.length)
          _nums(i) = list.remove(idx)
        }

        _nums
      }

      private def copyArray(a: Array[Int]): Array[Int] = {
        val newArray = new Array[Int](a.length)
        copyArray(a, newArray)
        newArray
      }

      private def copyArray(a1: Array[Int], a2: Array[Int]): Unit = {
        (0 to a1.length - 1).foreach { idx =>
          a2(idx) = a1(idx)
        }
      }

    }

    val sol = new Solution(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

    sol.shuffle()

  }

  "Coin change" should "work" in {

    def coinNumber(coins: Map[Int, Int]): Int = coins.values.sum

    def bestSolution(
                      coins: List[Int],
                      amount: Int,
                      usedCoins: List[Int] = List.empty): Option[Map[Int, Int]] = {
      if (amount == 0)
        Some(usedCoins.groupBy(identity).map { case (coin, list) => coin -> list.length })
      else
        coins
          .filter(_ <= amount)
          .foldLeft(None: Option[Map[Int, Int]]) { (result, coin) =>
            result.orElse {
              bestSolution(coins, amount - coin, usedCoins ++ List(coin))
            }
          }
    }

    def coinChange(coins: Array[Int], amount: Int): Int = {
      bestSolution(coins.toList.sorted.reverse, amount).map(coinNumber).getOrElse(-1)
    }

    //    allSolutions(coins = Set(1,2,5), amount = 11) shouldBe 1
    coinChange(coins = Array(1, 2, 5), amount = 100) shouldBe 4

  }

  "Fibonacci bottom-up" should "work" in {

    def fibonacci(n: Int): Int = {
      if (n == 0) 0
      else if (n == 1) 1
      else {
        val (_, result) = (2 to n).foldLeft((0, 1)) { case ((iMin2, iMin1), i) =>
          (iMin1, iMin2 + iMin1)
        }
        result
      }
    }

    fibonacci(0) shouldBe 0
    fibonacci(1) shouldBe 1
    fibonacci(2) shouldBe 1
    fibonacci(3) shouldBe 2
    fibonacci(4) shouldBe 3
  }

  "Fibonacci top-down" should "work" in {

    def fibonacci(n: Int): Int = {

      val memo = new Array[Int](n + 1)

      def fib(i: Int): Int =
        if (i == 0) 0
        else if (i == 1) 1
        else {
          if (memo(i) == 0) {
            memo(i) = fib(i - 1) + fib(i - 2)
          }
          memo(i)
        }

      fib(n)
    }

    fibonacci(0) shouldBe 0
    fibonacci(1) shouldBe 1
    fibonacci(2) shouldBe 1
    fibonacci(3) shouldBe 2
    fibonacci(4) shouldBe 3
  }

  "Jump game" should "work" in {
    // good, but consumes too much memory
    def canJump2(nums: Array[Int]): Boolean = {
      if (nums.size == 0) false
      else if (nums.size == 1) true
      else if (nums.head == 0) false
      else {
        val maxJump = nums.head
        if (maxJump >= nums.length - 1)
          true
        else
          (1 to maxJump).exists { jump => canJump(nums.drop(jump)) }
      }
    }

    def canJump(nums: Array[Int]): Boolean = {
      var maxReach = 0
      var i = 0
      while (i < nums.size && maxReach >= i) {
        maxReach = Math.max(i + nums(i), maxReach)
        i = i + 1
      }
      maxReach >= nums.size - 1
    }

    canJump(Array(2, 3, 1, 1, 4)) shouldBe true
    canJump(Array(3, 2, 1, 0, 4)) shouldBe false
  }

  "Number of unique paths" should "be found" in {
    def uniquePaths(m: Int, n: Int): Int = {

      // (x,y): position of the robot
      def pathsFromStartingPoint(x: Int, y: Int): Int = {
        //        println(s"Postition [$x, $y]")
        if ((x, y) == (m - 1, n - 1))
          1
        else {
          val startingDown = if (y < n - 1) {
            //            println("RIGHT")
            pathsFromStartingPoint(x, y + 1)
          } else 0
          val startingRight = if (x < m - 1) {
            //            println("DOWN")
            pathsFromStartingPoint(x + 1, y)
          } else 0
          startingDown + startingRight
        }
      }

      pathsFromStartingPoint(0, 0)
    }

    def uniquePaths2(m: Int, n: Int): Int = {

      val a = Array.ofDim[Int](m, n)
      (0 until m).foreach { i =>
        a(i)(0) = 1
      }
      (0 until n).foreach { i =>
        a(0)(i) = 1
      }

      for (y <- 1 until m; x <- 1 until n) {
        a(y)(x)= a(y - 1)(x) + a(y)(x - 1)
      }

      a(m-1)(n-1)

    }

    uniquePaths2(3, 2) shouldBe 3
    uniquePaths2(3, 7) shouldBe 28
  }

  "Longest Increasing Subsequence" should "be found in" in {
    def lengthOfLIS(nums: Array[Int]): Int = {

      var start = 0
      var max = 0
      (0 until nums.length).foreach { idx =>
        if ( idx == 0 || nums(idx) <= nums(idx - 1) ) {
          max = Math.max(max, idx - start)
          start = idx
        }
      }
      max
    }

    lengthOfLIS(Array(10,9,2,5,3,7,101,18)) shouldBe 4
    lengthOfLIS(Array(7,7,7)) shouldBe 1
    lengthOfLIS(Array(2, 1,2,3,3)) shouldBe 3
  }

}

