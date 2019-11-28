package home.datastructures

import home.datastructures.LinkedList._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable._

class LinkedListTest extends FlatSpec with Matchers {

  "Append element to linked list " should "work" in {

    val list: LinkedList[Int] = NonEmptyLinkedList(3, Empty)
    val list2: LinkedList[Int] = list.append(6)

    traverse(Empty) shouldBe Seq.empty
    traverse(list) shouldBe Seq(3)
    traverse(list2) shouldBe Seq(6, 3)
  }

}
