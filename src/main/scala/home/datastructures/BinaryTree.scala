package home.datastructures

import home.datastructures.BinaryTree.Tree

import scala.annotation.tailrec
import scala.collection.immutable._

object BinaryTree {

  final case class Tree[T](value: T, left: Option[Tree[T]] = None, right: Option[Tree[T]] = None)

  def traverseBreadthFirst[T](tree: Tree[T]): Seq[T] = {

    @tailrec
    def bfHelper[T](values: Seq[T], nextLayer: Seq[Tree[T]]): Seq[T] = {
      if (nextLayer.isEmpty) values
      else
        bfHelper(
          values = values ++ nextLayer.map(_.value),
          nextLayer = nextLayer.flatMap(t ⇒ Seq(t.left, t.right).flatten))
    }

    bfHelper(values = Seq.empty, nextLayer = Seq(tree))
  }

  def traverseBreadthFirstWithQueue[T](tree: Tree[T]): Seq[T] = {

    @tailrec
    def bfHelper[T](queue: Queue[T], nextLayer: Seq[Tree[T]]): Seq[T] = {
      if (nextLayer.isEmpty) queue.toList //pity there is no completeDeque method
      else
        bfHelper(
          queue = queue.enqueue(nextLayer.map(_.value)),
          nextLayer = nextLayer.flatMap(t ⇒ Seq(t.left, t.right).flatten))
    }

    bfHelper(queue = Queue.empty, nextLayer = Seq(tree))
  }

  //DFS can be implemented with recursion
  def traversePreorderDepthFirst[T](tree: Tree[T]): Seq[T] = {
    val traverse = liftTraverse(traversePreorderDepthFirst[T])

    tree.value +: (traverse(tree.left) ++ traverse(tree.right))
  }

  def traversePostOrderDepthFirst[T](tree: Tree[T]): Seq[T] = {
    val traverse = liftTraverse(traversePostOrderDepthFirst[T])

    traverse(tree.left) ++ traverse(tree.right) :+ tree.value
  }

  def traverseInOrderDepthFirst[T](tree: Tree[T]): Seq[T] = {
    val traverse = liftTraverse(traverseInOrderDepthFirst[T])

    traverse(tree.left) ++ (tree.value +: traverse(tree.right))
  }

  private def liftTraverse[A, T](traverseFn: A ⇒ Seq[T]): Option[A] ⇒ Seq[T] =
    t ⇒ t.map(traverseFn).getOrElse(Seq.empty)

}

object BinarySearchTree {
  import scala.math.Ordering.Implicits._

  def addToBST[T](tree: Tree[T], element: T)(implicit ord: Ordering[T]): Tree[T] = {

    def addToChild(child: Option[Tree[T]]): Option[Tree[T]] =
      child.map(t ⇒ addToBST(t, element)).orElse(Some(Tree(element)))

    if (tree.value == element) tree
    else if (tree.value < element) tree.copy(right = addToChild(tree.right))
    else tree.copy(left = addToChild(tree.left))
  }

  def buildBST[T](first: T, rest: T*)(implicit ord: Ordering[T]): Tree[T] =
    rest.foldLeft(Tree(first))(addToBST)

  def findSubTree[T](tree: Tree[T], elem: T)(implicit ord: Ordering[T]): Option[Tree[T]] =
    if (elem == tree.value) Some(tree)
    else if (elem < tree.value) tree.left.flatMap(findSubTree(_, elem))
    else tree.right.flatMap(findSubTree(_, elem))

  def removeElement[T](tree: Tree[T], elem: T)(implicit ord: Ordering[T]): Option[Tree[T]] =
    tree match {
      case Tree(`elem`, None, None) ⇒ None //leaf or empty root
      case Tree(`elem`, sl @ Some(_), None) ⇒ sl
      case Tree(`elem`, None, sr @ Some(_)) ⇒ sr
      case Tree(`elem`, Some(left), Some(right)) ⇒
        val replacement = biggest(left) //biggest of the left or smallest of the right
        Some(Tree(replacement, removeElement(left, replacement), Some(right)))
      case Tree(v, left, right) ⇒
        Some(Tree(v, left.flatMap(removeElement(_, elem)), right.flatMap(removeElement(_, elem))))
      case _ ⇒ None
    }

  private def smallest[T](tree: Tree[T])(implicit ord: Ordering[T]): T =
    tree match {
      case Tree(value, None, _) ⇒ value
      case Tree(_, Some(left), _) ⇒ smallest(left)
    }

  private def biggest[T](tree: Tree[T])(implicit ord: Ordering[T]): T =
    tree match {
      case Tree(value, _, None) ⇒ value
      case Tree(_, _, Some(right)) ⇒ biggest(right)
    }
}
