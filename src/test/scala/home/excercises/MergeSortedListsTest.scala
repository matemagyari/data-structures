package home.excercises

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class MergeSortedListsTest extends FlatSpec with Matchers {

  @tailrec
  private def merge(
      sortedListA: List[Int],
      sortedListB: List[Int],
      acc: List[Int] = List.empty): List[Int] =
    (sortedListA, sortedListB) match {
      case (Nil, _) ⇒ acc ++ sortedListB
      case (_, Nil) ⇒ acc ++ sortedListA
      case (headA :: tailA, headB :: tailB) ⇒
        if (headA > headB) merge(sortedListA = sortedListA, sortedListB = tailB, acc = acc :+ headB)
        else merge(sortedListA = tailA, sortedListB = sortedListB, acc = acc :+ headA)
    }

  "MergeSortedLists" should "work" in {

    {
      val result =
        merge(sortedListA = List(1, 3, 5, 9, 12), sortedListB = List(2, 4, 7, 9, 14, 15, 16))

      result shouldBe List(1, 2, 3, 4, 5, 7, 9, 9, 12, 14, 15, 16)
    }

    merge(sortedListA = List(1, 3, 5), sortedListB = List()) shouldBe List(1, 3, 5)
  }
}
