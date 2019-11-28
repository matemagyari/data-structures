package home.datastructures

import home.datastructures.LinkedList.NonEmptyLinkedList
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.{Seq, Stack}

class MiscellaneousTest extends FlatSpec with Matchers {

  "K largest elements from a big file or array" should "work" in {

    def kMaxValues(k: Int, numbers: Iterable[Int]): Array[Int] =
      numbers.foldLeft(Array.empty[Int]) {
        case (maxK, num) ⇒
          if (maxK.headOption.exists(min ⇒ min > num))
            maxK
          else
            (maxK :+ num).sortWith(_ < _).takeRight(k)
      }

    kMaxValues(1, Iterable.empty) shouldBe Array.empty
    kMaxValues(0, Iterable(1)) shouldBe Array.empty
    kMaxValues(5, Iterable(1, 2)) shouldBe Array(1, 2)
    kMaxValues(2, Iterable(1, 4, 3, 5, 2)) shouldBe Array(4, 5)
    kMaxValues(4, Iterable(1, 6, 2, 7, 3, 8, 4, 9)) shouldBe Array(6, 7, 8, 9)

  }

  "Reverse a linked list in groups" should "work" in {
    import LinkedList._
    def reverse[T](linkedList: LinkedList[T], k: Int): LinkedList[T] = {

      def rev[T](temp: List[T], in: LinkedList[T], out: LinkedList[T]): LinkedList[T] = {

        println(s"Temp $temp")
        println(s"in $in")
        println(s"out $out")

        def addTempToOut() = temp.foldLeft(out) { (acc, elem) ⇒
          acc.append(elem)
        }
        in match {
          case Empty ⇒ addTempToOut()
          case _ if temp.size == k ⇒ rev(List.empty, in, addTempToOut())
          case NonEmptyLinkedList(head: T, tail: LinkedList[T]) ⇒
            rev(temp :+ head, tail: LinkedList[T], out)
        }
      }

      rev(List.empty, linkedList, Empty)
    }

    println(s"AAA ${build(1, 2, 3)}")

//    reverse(Empty: LinkedList[Int], k = 2) shouldBe Empty
//    reverse(build(1), k = 3) shouldBe build(1)
//    reverse(build(1, 2), k = 3) shouldBe build(2, 1)
    reverse(build(1, 2, 3), k = 2) shouldBe build(2, 1, 3)
//    reverse(build(1, 2, 3, 4, 5, 6, 7), k = 2) shouldBe build(2, 1, 4, 3, 6, 5, 7)
//    reverse(build(1, 2, 3, 4), k = 3) shouldBe build(3, 2, 1, 4)

  }

  "Pythagorian triplets in an awway" should "work" in {

    def pyth(numbers: Array[Int]): Seq[(Int, Int, Int)] = {
      val sortedNums = numbers.sorted //O(nlogn)
      val sortedSquares = sortedNums.map(e => e * e) //O(n)
      val hash: Map[Int, Int] = (0 to sortedSquares.length - 1).map { index ⇒
        (sortedSquares(index), index)
      }.toMap //O(n)

      val indexes = for {
        index1 ← (0 to sortedNums.length - 1)
        index2 ← (index1 + 1 to sortedNums.length - 1)
        sum = sortedSquares(index1) + sortedSquares(index2)
        index3 ← hash.get(sum)
      } yield (index1, index2, index3) //O(n^2)

      indexes.map {
        case (i1, i2, i3) ⇒
          (
            sortedNums(i1),
            sortedNums(i2),
            sortedNums(i3)
          )
      }
    }

    pyth(Array(3, 1, 4, 6, 5)) shouldBe Seq((3, 4, 5))
    pyth(Array(10, 4, 6, 12, 5)) shouldBe Seq.empty
  }

  "FizzBuzz" should "work" in {

    def fizzBuzz2(i: Int): String =
      if (i % 15 == 0) "FizzBuzz"
      else if (i % 5 == 0) "Buzz"
      else if (i % 3 == 0) "Fizz"
      else
        i.toString

    def fizzBuzz(i: Int): String =
      (i % 5 == 0, i % 3 == 0) match {
        case (true, true) ⇒ "FizzBuzz"
        case (true, false) ⇒ "Buzz"
        case (false, true) ⇒ "Fizz"
        case (false, false) ⇒ i.toString
      }

    fizzBuzz(0) shouldBe "FizzBuzz"
    fizzBuzz(1) shouldBe "1"
    fizzBuzz(3) shouldBe "Fizz"
    fizzBuzz(5) shouldBe "Buzz"
    fizzBuzz(15) shouldBe "FizzBuzz"
    fizzBuzz(16) shouldBe "16"
    fizzBuzz(18) shouldBe "Fizz"
    fizzBuzz(20) shouldBe "Buzz"
    fizzBuzz(30) shouldBe "FizzBuzz"
  }

}
