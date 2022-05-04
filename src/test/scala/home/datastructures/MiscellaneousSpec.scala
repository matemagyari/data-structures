package home.datastructures

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MiscellaneousSpec extends AnyFlatSpec with Matchers with OptionValues {

  "FizzBuzz" should "work" in {
    def fb(n: Int): String = {
      def div3 = n % 3 == 0
      def div5 = n % 5 == 0
      if (div3 && div5)
        "FizzBuzz"
      else if (div3)
        "Fizz"
      else if (div5)
        "Buzz"
      else
        n.toString
    }

    def fizzBuzz(n: Int): List[String] =
      (1 until n).toList.map(fb)
  }

  "Count primes" should "work" in {
    def isPrime(n: Int, primes: List[Int]): Boolean = {
      !primes
//        .forall { prime => prime * prime > n || n % prime != 0 }
        .exists { prime => prime * prime <= n && n % prime == 0 }
    }

    def countPrimes(n: Int): Int = {

      def helper(num: Int, primes: List[Int]): List[Int] =
        if (num >= n) primes
        else {
//          if (num % 10000 == 0) println(s"Hello $num")
          val updatedPrimes = if (isPrime(num, primes)) primes ++ List(num)  else primes
          helper(num+1, updatedPrimes)
        }

      helper(2, List.empty).size

      // iterative solution
//      val foundPrimes = scala.collection.mutable.Set.empty[Int]
//      (2 until n).foreach { num =>
//        if (isPrime(num, foundPrimes.toSet)) {
//          foundPrimes += num
//        }
//      }
//      foundPrimes.size
    }

    countPrimes(2) shouldBe 0
    countPrimes(3) shouldBe 1
    countPrimes(6) shouldBe 3
    countPrimes(8) shouldBe 4
    countPrimes(12) shouldBe 5
    countPrimes(499979) shouldBe 41537
  }

  "isPowerOfThree" should "work" in {
    def isPowerOfThree(n: Int): Boolean = {

      def helper(x: Int): Boolean = {
        val n3 = Math.pow(3, x)
        (n3 == n) || (n3 <= n) && helper(x + 1)
      }

      def helper2(x: Int): Boolean = {
        val n3 = Math.pow(3, x)
        if (n3 > n)
          false
        else if (n3 == n)
          true
        else
          helper2(x + 1)
      }

      helper(0)
    }

    isPowerOfThree(1) shouldBe true
    isPowerOfThree(2) shouldBe false
    isPowerOfThree(3) shouldBe true
    isPowerOfThree(9) shouldBe true
    isPowerOfThree(10) shouldBe false
    isPowerOfThree(27) shouldBe true
  }

  "Roman to int" should "work" in {
    def romanToInt(s: String): Int = {

      val singles: Map[Char, Int] = Map(
        'I' -> 1,
        'V' -> 5,
        'X' -> 10,
        'L' -> 50,
        'C' -> 100,
        'D' -> 500,
        'M' -> 1000)

      val doubles: Map[String, Int] = Map(
        "IV" -> 4,
        "IX" -> 9,
        "XL" -> 40,
        "XC" -> 90,
        "CD" -> 400,
        "CM" -> 900)

      def helper(str: String, sum: Int): Int = {
        if (str.isEmpty) sum
        else
          doubles.get(str.take(2)) match {
            case Some(value) => helper(str.drop(2), sum + value)
            case None => helper(str.drop(1), sum + singles(str(0)))
          }
      }

      helper(s, 0)
    }

    romanToInt("MCMXCIV") shouldBe 1994
    romanToInt("LVIII") shouldBe 58
  }

  "Hamming weight" should "work" in {

    def hammingWeight(n: Int): Int = {

      var currentNum = if (n >= 0) n else { println(s"Negative number [$n]");Math.abs(Integer.MIN_VALUE -n) }

      val len = (Math.log(currentNum) / Math.log(2)).toInt + 1
      val result = new Array[Int](len)

      (0 to len).foreach { idx =>
        val p = len - idx
        val x = Math.pow(2, p).toInt
        if (x <= currentNum) {
          currentNum -= x
          result(p) = 1
        }
      }

      //println(s"n: $n result: ${result.mkString}")
      result.sum
    }

    hammingWeight(1) shouldBe 1
    hammingWeight(2) shouldBe 1
    hammingWeight(3) shouldBe 2
    hammingWeight(4) shouldBe 1
    hammingWeight(5) shouldBe 2
    hammingWeight(6) shouldBe 2
    hammingWeight(7) shouldBe 3
    hammingWeight(-3) shouldBe 3
  }

  "Randomized set operations" should "run in O(1) time" in {
    class RandomizedSet() {

      import scala.util.Random

      private val map: scala.collection.mutable.Map[Integer, Integer] = scala.collection.mutable.Map.empty
      private var list: Array[Integer] = Array.empty

      def insert(elem: Int): Boolean = {
        val isNew = !map.contains(elem)
        if (isNew) {
          list = list.appended(elem)
          map.put(elem, list.size - 1)
        }
        isNew
      }

      def remove(elem: Int): Boolean = {
        val arrayIdx: Option[Integer] = map.get(elem)

        arrayIdx.foreach { idx =>
          val lastArrayElem = list(list.size - 1)
          list(idx) = lastArrayElem
          list = list.slice(0, list.size - 1)
          map += lastArrayElem -> idx
          map.remove(elem)
        }

        arrayIdx.isDefined
      }

      def getRandom(): Int = {
        list(Random.nextInt(list.size))
      }

    }

    println("hello")

    //      ["RandomizedSet","insert","insert","remove","insert","remove","getRandom"]
    //      [[],[0],[1],[0],[2],[1],[]]
    {
      val r = new RandomizedSet()
      r.insert(0) shouldBe true
      r.insert(1) shouldBe true
      r.remove(0) shouldBe true
      r.insert(2) shouldBe true
      r.remove(1) shouldBe true
      r.getRandom() shouldBe 2
    }


  }



}
