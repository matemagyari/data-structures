package home.excercises

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

trait MyQueue[T] {
  def enqueue(elem: T): Unit
  def dequeue(): Option[T]
}

class MyQueueWith2Stacks[T]() extends MyQueue[T] {

  import mutable.Stack

  val stack1: Stack[T] = new Stack()
  val stack2: Stack[T] = new Stack()

  def enqueue(elem: T): Unit = {
    copyFromOneToAnother(from = stack1, to = stack2)

    stack1.push(elem)

    copyFromOneToAnother(from = stack2, to = stack1)
  }

  override def dequeue(): Option[T] =
    if (stack1.isEmpty) None else Some(stack1.pop())

  private def copyFromOneToAnother(from: Stack[T], to: Stack[T]): Unit = {
    while (!from.isEmpty) {
      to.push(from.pop())
    }
  }
}

class QueueImplementationTest extends FlatSpec with Matchers {

  "Queue" should "work" in {

    val queue = new MyQueueWith2Stacks[Int]()

    queue.dequeue() shouldBe None

    queue.enqueue(1)
    queue.enqueue(2)

    queue.dequeue() shouldBe Some(1)
    queue.dequeue() shouldBe Some(2)
    queue.dequeue() shouldBe None

  }
}
