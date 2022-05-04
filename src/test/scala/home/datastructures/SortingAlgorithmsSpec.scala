package home.datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.util.Random

class SortingAlgorithmsSpec extends AnyFlatSpec with Matchers {

  "Quicksort" should "work in O(nlog(n)) time" in {
    def quicksort[T](elements: List[T])(implicit ordering: Ordering[T]): List[T] =
      if (elements.size < 2)
        elements
      else {
        val pivotIndex = elements.size / 2
        val pivot = elements(pivotIndex)
        val rest = elements.take(pivotIndex) ++ elements.drop(pivotIndex + 1)
        val (less, greater) = rest.partition { e => ordering.lt(e, pivot) }
        quicksort(less) ++ List(pivot) ++ quicksort(greater)
      }

    quicksort(List.empty[Int]) shouldBe List.empty[Int]
    quicksort(List("c", "a", "b")) shouldBe List("a", "b", "c")
    quicksort(Random.shuffle((1 to 5).toList)) shouldBe List(1, 2, 3, 4, 5)
  }

  "Merge sort arrays" should "work" in {

    def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {

      var i1 = m - 1
      var i2 = n - 1
      var idx = m + n - 1

      while (i2 >= 0) {
        if (i1 >= 0 && nums1(i1) > nums2(i2)) {
          nums1(idx) = nums1(i1)
          i1 -= 1
        }
        else {
          nums1(idx) = nums2(i2)
          i2 -= 1
        }
        //println(s"idx $idx nums1(idx) ${nums1(idx)}")
        idx -= 1
      }
    }

    {
      val n1 = Array(1, 3, 5, 0, 0, 0)
      val n2 = Array(2, 4, 6)
      merge(n1, 3, n2, 3)

      n1 shouldBe Array(1, 2, 3, 4, 5, 6)
    }

    {
      val n1 = Array(1, 3, 0, 0, 0)
      val n2 = Array(2, 4, 6)
      merge(n1, 2, n2, 3)

      n1 shouldBe Array(1, 2, 3, 4, 6)
    }

    {
      val n1 = Array(0)
      val n2 = Array(1)
      merge(n1, 0, n2, 1)

      n1 shouldBe Array(1)
    }

  }

  "First bad version" should "work" in {

    def isBadVersion(version: Int): Boolean = {
      version >= 1702766719
    }

    def helper(low: Int, high: Int): Int = {
      if (low == high) low
      else {
        val half = (high - low) / 2 + low
        if (isBadVersion(half)) {
          if (half == high)
            helper(low, half - 1)
          else
            helper(low, half)
        } else {
          if (half == low)
            helper(half + 1, high)
          else
            helper(half, high)
        }
      }
    }

    def firstBadVersion(n: Int): Int = helper(0, n)

    firstBadVersion(2126753390) shouldBe 1702766719
  }

  "Top K Frequent Elements" should "work" in {
    def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {
      type Elem = Int
      val map: Map[Elem, Int] = nums
        .foldLeft(Map.empty[Elem, Int]) { (occurrences, elem) =>
          val num = occurrences.getOrElse(elem, 0) + 1
          occurrences + (elem -> num)
        }
      map.toArray.sortBy(_._2).takeRight(k).map(_._1)
    }

  }

  "Sort colors" should "work in O(n) time and constant space" in {
    def sortColors(nums: Array[Int]): Unit = {
      type Color = Int
      val map: Map[Color, Int] = nums
        .foldLeft(Map.empty[Color, Int]) { (occurrences, color) =>
          val num = occurrences.getOrElse(color, 0) + 1
          occurrences + (color -> num)
        }

      val zeros = map.getOrElse(0, 0)
      val ones = map.getOrElse(1, 0)
      val twos = map.getOrElse(2, 0)

      (0 until zeros).foreach { idx =>
        nums(idx) = 0
      }

      (0 until ones).foreach { idx =>
        nums(zeros + idx) = 1
      }

      (0 until twos).foreach { idx =>
        nums(zeros + ones + idx) = 2
      }
    }

    val a1 = Array(1)
    sortColors(a1)
    a1 shouldBe Array(1)

    val a2 = Array(2, 0, 1)
    sortColors(a2)
    a2 shouldBe Array(0, 1, 2)

    val a3 = Array(2, 0, 0, 1)
    sortColors(a3)
    a3 shouldBe Array(0, 0, 1, 2)
  }

  "Find k-th largest element in an array" should "work" in {

    def shiftLeft(index: Int, ns: Array[Int], newElement: Int): Unit = {
      (0 to index - 1).foreach { idx =>
        ns(idx) = ns(idx + 1)
      }
      ns(index) = newElement
    }

    // ns is in ascending order
    def insertInOrder(ns: Array[Int], n: Int): Unit = {
      if (n > ns.head) {
        val index = ns.zipWithIndex.collectFirst { case (num, i) if num >= n => i }.getOrElse(ns.size)
        shiftLeft(index - 1, ns, n)
      }
    }

    def findKthLargest(nums: Array[Int], k: Int): Int = {
      val ks = new Array[Int](k)
      (0 until k).foreach { i => ks(i) = Integer.MIN_VALUE }

      def addIfFits(n: Int): Unit = {
        if (ks.head < n) {
          insertInOrder(ks, n)
        }
      }

      nums.foreach(addIfFits)

      ks.head

    }

    {
      val arr = Array(1, 2, 3)
      shiftLeft(2, arr, 4)
      arr shouldBe Array(2, 3, 4)
    }

    {
      val arr = Array(1, 2, 3)
      shiftLeft(1, arr, 4)
      arr shouldBe Array(2, 4, 3)
    }

    {
      val arr = Array(1, 3, 5)
      insertInOrder(arr, 4)
      arr shouldBe Array(3,4,5)
    }

    {
      val arr = Array(1, 3, 5)
      insertInOrder(arr, 6)
      arr shouldBe Array(3, 5, 6)
    }

    {
      val arr = Array(1, 3, 5)
      insertInOrder(arr, 0)
      arr shouldBe Array(1, 3, 5)
    }

    findKthLargest(Array(2,4,6), 1) shouldBe 6
    findKthLargest(Array(2,4,6), 2) shouldBe 4
    findKthLargest(Array(2,4,6), 3) shouldBe 2

  }
}
