package home.datastructures

import scala.collection.immutable.Seq

sealed trait LinkedList[+T] {
  def head: T
  def tail: LinkedList[T]
  def append[B >: T](elem: B): LinkedList[B]
}
object LinkedList {

  final case object Empty extends LinkedList[Nothing] {
    override def head = throw new NoSuchElementException("MyNil.head")
    override def tail = throw new NoSuchElementException("MyNil.tail")
    override def append[B >: Nothing](elem: B): LinkedList[B] = NonEmptyLinkedList(elem, Empty)
  }
  final case class NonEmptyLinkedList[T](head: T, tail: LinkedList[T]) extends LinkedList[T] {
    override def append[B >: T](elem: B): LinkedList[B] = NonEmptyLinkedList(elem, this)
  }

  def traverse[T](list: LinkedList[T]): Seq[T] = list match {
    case Empty ⇒ Seq.empty
    case NonEmptyLinkedList(head, tail) ⇒ head +: traverse(tail)
  }

  def build[T](elements: T*): LinkedList[T] =
    elements.reverse.foldLeft(Empty: LinkedList[T]) { (acc, elem) ⇒
      acc.append(elem)
    }
}
