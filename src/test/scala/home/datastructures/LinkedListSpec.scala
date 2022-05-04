package home.datastructures

import home.datastructures.LinkedList._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable._
import scala.collection.mutable
import scala.reflect.ClassTag

class LinkedListSpec extends AnyFlatSpec with Matchers with OptionValues {

  // mutable LinkedList

  class Node[T](val value: T) {
    var next: Option[Node[T]] = None

    def appendToTail(elem: T): Unit = next match {
      case None => next = Some(new Node(elem))
      case Some(next) => next.appendToTail(elem)
    }

    def traverse(): List[T] = {
      val rest: List[T] = next match {
        case None => List.empty
        case Some(n) => n.traverse()
      }
      value +: rest
    }
  }

  object Node {
    def build[T](head: T, rest: T*): Node[T] = {
      val node = new Node(head)
      rest.foreach { e => node.appendToTail(e) }
      node
    }
  }

  "Append element to linked list " should "work" in {

    val list: LinkedList[Int] = NonEmptyLinkedList(3, Empty)
    val list2: LinkedList[Int] = list.append(6)

    traverse(Empty) shouldBe Seq.empty
    traverse(list) shouldBe Seq(3)
    traverse(list2) shouldBe Seq(6, 3)
  }

  "Append element to mutable linked list " should "work" in {

    val node = new Node(1)

    node.appendToTail(2)
    node.appendToTail(3)
    node.appendToTail(4)

    node.traverse() shouldBe List(1, 2, 3, 4)
    new Node(3).traverse() shouldBe List(3)
  }

  "Remove duplicates from an unsorted mutable linked list" should "work" in {
    def removeDuplicates[T](node: Node[T]): Unit = {

      val uniqueElements: mutable.Set[T] = mutable.Set.empty[T]

      def nextNew(node: Node[T]): Option[Node[T]] =
        if (uniqueElements.contains(node.value))
          node.next.flatMap(nextNew)
        else Some(node)


      def helper(n: Node[T]): Unit = {
        uniqueElements += n.value
        n.next = nextNew(n)
        n.next.foreach(helper)
      }

      helper(node)
    }

    val node = new Node(3)
    node.appendToTail(2)
    node.appendToTail(3)
    node.appendToTail(4)
    node.appendToTail(2)

    removeDuplicates(node)

    node.traverse() shouldBe List(3, 2, 4)
  }

  "Get kth to the last of an mutable linked list" should "work" in {
    def getKthToLast[T](node: Node[T], k: Int)(implicit m: ClassTag[T]): Option[T] =
      if (k < 0)
        None
      else {
        // use Option so it initializes as null, not some valid value
        val buffer: Array[Option[T]] = new Array[Option[T]](k + 1)

        def rotateArray(elem: T): Unit = {
          (0 to k - 1).foreach { i =>
            buffer(i) = buffer(i + 1)
          }

          buffer(k) = Option(elem)
        }

        def goOn(n: Node[T]): Unit = {
          rotateArray(n.value)
          n.next.foreach(goOn)
        }

        goOn(node)

        buffer.headOption.filter(_ != null).flatten
      }

    val node = new Node(1)
    node.appendToTail(2)
    node.appendToTail(3)

    getKthToLast(node, -1) shouldBe None
    getKthToLast(node, 0).value shouldBe 3
    getKthToLast(node, 1).value shouldBe 2
    getKthToLast(node, 2).value shouldBe 1
    getKthToLast(node, 3) shouldBe None
  }

  "Delete nth element of mutable linked list" should "work" in {

    // ugly
    def delete2[T](node: Node[T], k: Int): Unit = {
      var m = 1
      var found = false
      var currentNode: Option[Node[T]] = Some(node)
      while (!found) {
        if (m == k - 1) {
          currentNode.foreach { c =>
            c.next = c.next.flatMap(_.next)
          }
          found = true
        } else {
          currentNode = currentNode.flatMap(_.next)
        }
        m += 1
      }
    }

    def findKth[T](node: Node[T], k: Int): Option[Node[T]] =
      if (k == 1) Some(node)
      else node.next.flatMap { n => findKth(n, k - 1) }

    // nice
    def delete[T](node: Node[T], k: Int): Unit = {
      val kBut1 = findKth(node, k - 1)
      kBut1.foreach { n =>
        n.next = n.next.flatMap(_.next)
      }
    }

    val n1 = Node.build(1, 2, 3, 4)
    delete(n1, 2)
    n1.traverse() shouldBe List(1, 3, 4)

    val n2 = Node.build(1, 2, 3, 4)
    delete(n2, 3)
    n2.traverse() shouldBe List(1, 2, 4)
  }

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  //  https://leetcode.com/explore/interview/card/top-interview-questions-medium/107/linked-list/783/
  "Add two numbers" should "work" in {
    object ListNodeBuilder {
      def build(num: Int): ListNode = {
        val digits = {
          val str = num.toString
          val len = str.length
          (0 to len - 1).map { index =>
            str(len - 1 - index).toString.toInt
          }
        }

        val firstNode = new ListNode(digits.head)
        digits.drop(1).foldLeft(firstNode) { case (node, digit) =>
          val next = new ListNode(digit)
          node.next = next
          next
        }
        firstNode
      }

      def decode(node: ListNode, acc: List[Int] = List.empty): List[Int] = {
        if (node == null) acc
        else decode(node.next, node.x +: acc)
      }

    }

    import ListNodeBuilder._

    def len(node: ListNode, acc: Int = 0): Int =
      if (node == null) acc
      else len(node.next, acc + 1)

    def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {

      val (bNode, sNode) =
        if (len(l1) > len(l2)) (l1, l2)
        else (l2, l1)

      val sumNode = new ListNode(0)
      var sumCurrent: ListNode = null
      var bCurrent = bNode
      var sCurrent = sNode
      var remainder = 0
      while (bCurrent != null) {
        val smaller = if (sCurrent == null) 0
        else {
          val x = sCurrent.x
          sCurrent = sCurrent.next
          x
        }

        val realSum = smaller + bCurrent.x + remainder

        val sum = if (realSum > 9) {
          remainder = 1
          realSum - 10
        } else {
          remainder = 0
          realSum
        }

        if (sumCurrent == null) {
          sumNode.x = sum
          sumCurrent = sumNode
        } else {
          val next = new ListNode(sum)
          sumCurrent.next = next
          sumCurrent = next
        }

        bCurrent = bCurrent.next
      }

      if (remainder == 1) {
        sumCurrent.next = new ListNode(1)
      }

      sumNode

    }


    //addTwoNumbers(build(0), build(0)) shouldBe build(0)
    //    addTwoNumbers(build(123), build(234)) shouldBe build(357)

    println(s"AAA ${decode(build(1234))}")
    decode(addTwoNumbers(build(1234), build(52))) shouldBe List(1, 2, 8, 6)
    decode(addTwoNumbers(build(1237), build(97))) shouldBe List(1, 3, 3, 4)
    decode(addTwoNumbers(build(999), build(9))) shouldBe List(1, 0, 0, 8)

  }

  "Delete node" should "work" in {
    class ListNode(var _x: Int = 0) {
      var next: ListNode = null
      var x: Int = _x
    }

    def deleteNode(node: ListNode): Unit = {
      val next = node.next
      node.x = next.x
      node.next = next.next
    }
  }

  "Delete node" should "work - 2" in {
    class ListNode(_x: Int = 0, _next: ListNode = null) {
      var next: ListNode = _next
      var x: Int = _x
    }

    def build(nums: List[Int]): ListNode = {

      val start = new ListNode(nums.head)
      nums.drop(1).foldLeft(start) { case (node, num) =>
        val next = new ListNode(num)
        node.next = next
        next
      }
      start
    }

    def removeNthFromEnd(head: ListNode, n: Int): ListNode = {

      if (head.next == null) null
      else {

        def findNPlus1thFromStart(current: ListNode, d: Int = 1): ListNode =
          if (d == n + 1) current
          else findNPlus1thFromStart(current.next, d + 1)

        val nPlus1thFromStart = findNPlus1thFromStart(head)

        // if the list has 5 elements and n is 5, then the first element should be removed
        if (nPlus1thFromStart == null)
          head.next
        else {

          def findNPlus1thFromEnd(current: ListNode, aheadWithN: ListNode): ListNode =
            if (aheadWithN == null || aheadWithN.next == null) current
            else findNPlus1thFromEnd(current.next, aheadWithN.next)

          val nPlus1thFromEnd = findNPlus1thFromEnd(head, nPlus1thFromStart)

          val nMin1FromEnd = nPlus1thFromEnd.next.next
          nPlus1thFromEnd.next = nMin1FromEnd

          head
        }

      }
    }

    val result1 = removeNthFromEnd(build(List(1, 2)), 1)
    val result2 = removeNthFromEnd(build(List(1, 2)), 2)
    removeNthFromEnd(build(List(1)), 1) shouldBe null
    val result = removeNthFromEnd(build(List(1, 2, 3, 4, 5)), 2)
    result shouldBe 1
  }

  "Reverse linked list" should "work" in {
    class ListNode(_x: Int = 0, _next: ListNode = null) {
      var next: ListNode = _next
      var x: Int = _x
    }

    def build(nums: List[Int]): ListNode = {

      val start = new ListNode(nums.head)
      nums.drop(1).foldLeft(start) { case (node, num) =>
        val next = new ListNode(num)
        node.next = next
        next
      }
      start
    }

    def bubble(current: ListNode, next: ListNode): ListNode = {
      if (next == null)
        current
      else {
        val nextNext = next.next
        next.next = current
        bubble(next, nextNext)
      }
    }

    def reverseList(head: ListNode): ListNode = {
      if (head == null) null
      else {
        val second = head.next
        head.next = null
        bubble(head, second)
      }
    }

    val result = reverseList(build(List(1, 2, 3, 4)))
    val result2 = reverseList(build(List(2)))
  }

  "Merge two sorted linked list" should "work" in {
    class ListNode(_x: Int = 0, _next: ListNode = null) {
      var next: ListNode = _next
      var x: Int = _x
    }

    def build(nums: List[Int]): ListNode = {

      val start = new ListNode(nums.head)
      nums.drop(1).foldLeft(start) { case (node, num) =>
        val next = new ListNode(num)
        node.next = next
        next
      }
      start
    }

    def helper(l1: ListNode, l2: ListNode, acc: ListNode): Unit = {
      if (l1 == null)
        acc.next = l2
      else if (l2 == null)
        acc.next = l1
      else if (l2.x > l1.x) {
        acc.next = new ListNode(l1.x)
        helper(l1.next, l2, acc.next)
      }
      else {
        acc.next = new ListNode(l2.x)
        helper(l1, l2.next, acc.next)
      }
    }

    def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
      if (l1 == null) l2
      else if (l2 == null) l1
      else if (l1.x < l2.x) {
        val head = new ListNode(l1.x)
        helper(l1.next, l2, head)
        head
      }
      else {
        val head = new ListNode(l2.x)
        helper(l1, l2.next, head)
        head
      }
    }

    val result = mergeTwoLists(build(List(1, 3, 5)), build(List(2, 4)))
    println(result)
  }

  "Is palindrome" should "work" in {
    class ListNode(_x: Int = 0, _next: ListNode = null) {
      var next: ListNode = _next
      var x: Int = _x
    }

    def build(nums: List[Int]): ListNode = {

      val start = new ListNode(nums.head)
      nums.drop(1).foldLeft(start) { case (node, num) =>
        val next = new ListNode(num)
        node.next = next
        next
      }
      start
    }

    def isPalindrome(head: ListNode): Boolean = {

      val stack = scala.collection.mutable.Stack.empty[Int]

      var current = head
      var size = 0
      while (current != null) {
        stack.push(current.x)
        size += 1
        current = current.next
      }

      var isPalindrome = true
      current = head
      for {_ <- 0 to size / 2 if isPalindrome} {
        isPalindrome = current.x == stack.pop()
        current = current.next
      }
      isPalindrome
    }

    isPalindrome(build(List(1))) shouldBe true
    isPalindrome(build(List(1, 2, 2))) shouldBe false
    isPalindrome(build(List(1, 2, 2, 1))) shouldBe true
  }

  "has cycle" should "work" in {
    class ListNode(_x: Int = 0, _next: ListNode = null) {
      var next: ListNode = _next
      var x: Int = _x
    }

    def hasCycle(head: ListNode): Boolean = {

      val nodes = mutable.Set.empty[ListNode]
      var current = head
      var cycleless = true
      while (current != null && cycleless) {
        cycleless = !nodes.contains(current)
        nodes += current
        current = current.next
      }

      !cycleless
    }

    val n1 = new ListNode(1)
    val n2 = new ListNode(2)
    val n3 = new ListNode(3)

    n1.next = n2
    n2.next = n3

    hasCycle(n1) shouldBe false

    n3.next = n1

    hasCycle(n1) shouldBe true
  }

  "odd even list" should "be created on O(n) time and O(1) space" in {
    class ListNode(_x: Int = 0, _next: ListNode = null) {
      var next: ListNode = _next
      var x: Int = _x
    }

    def oddEvenList(head: ListNode): ListNode = {

      if (head == null) null
      else if (head.next == null) head
      else {

        val oddListHead = head
        var oddListLastElement = head
        val evenListHead = head.next
        var evenListLastElement = evenListHead

        var idx = 3
        var current = evenListLastElement.next
        while (current != null) {
          println(s"idx: $idx Current: ${current.x}")
          if (idx % 2 == 0) {
            evenListLastElement.next = current
            evenListLastElement = current
          }
          else {
            oddListLastElement.next = current
            oddListLastElement = current
          }
          current = current.next
          idx += 1
        }

        evenListLastElement.next = null

        oddListLastElement.next = evenListHead
        oddListHead
      }
    }

    def build(nums: List[Int]): ListNode = {

      val start = new ListNode(nums.head)
      nums.drop(1).foldLeft(start) { case (node, num) =>
        val next = new ListNode(num)
        node.next = next
        next
      }
      start
    }

    val result = oddEvenList(build(List(1, 2, 3, 4, 5)))

    println(result)
  }
}
