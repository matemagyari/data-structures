package home.datastructures

import home.datastructures.BinaryTree._
import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scala.collection.immutable._
import BinarySearchTree._

class BinarySearchTreeTest extends FlatSpec with Matchers with OptionValues {

//  tree
//  ----
//             4    <-- root
//          /     \
//        1        6
//         \     /   \
//          3   5     9
//         /         /
//        2         7

  "Breadth-first traverse of binary tree " should "work" in {
    val tree: Tree[Int] = buildBST(4, 6, 9, 1, 5, 3, 2, 6, 7, 4)

    val expected = Seq(4, 1, 6, 3, 5, 9, 2, 7)
    traverseBreadthFirstWithQueue(tree) shouldBe expected
  }

  "Find subtree" should "work" in {
    val tree: Tree[Int] = buildBST(4, 6, 9, 1, 5, 3, 2, 6, 7, 4)

    findSubTree(tree, 4) shouldBe Some(tree)
    findSubTree(tree, 99) shouldBe None
    findSubTree(tree, 6) shouldBe Some(buildBST(6, 5, 9, 7))
    findSubTree(tree, 5) shouldBe Some(buildBST(5))
    findSubTree(tree, 1) shouldBe Some(buildBST(1, 3, 2))
  }

  "Remove subtree" should "work" in {
    val tree: Tree[Int] = buildBST(4, 6, 9, 1, 5, 3, 2, 6, 7, 4)

    {
      val result = removeElement(tree, 1).value
      val expected = Seq(4, 3, 6, 2, 5, 9, 7)
      traverseBreadthFirstWithQueue(result) shouldBe expected
    }

    {
      val result = removeElement(tree, 9).value
      val expected = Seq(4, 1, 6, 3, 5, 7, 2)
      traverseBreadthFirstWithQueue(result) shouldBe expected
    }

    {
      val result = removeElement(tree, 7).value
      val expected = Seq(4, 1, 6, 3, 5, 9, 2)
      traverseBreadthFirstWithQueue(result) shouldBe expected
    }

    {
      val result = removeElement(tree, 6).value
      val expected = Seq(4, 1, 5, 3, 9, 2, 7)
      traverseBreadthFirstWithQueue(result) shouldBe expected
    }

    {
      val result = removeElement(tree, 4).value

      //             3    <-- root
      //          /     \
      //        1        6
      //         \     /   \
      //          2   5     9
      //                   /
      //                  7

      val expected = Seq(3, 1, 6, 2, 5, 9, 7)
      traverseBreadthFirstWithQueue(result) shouldBe expected
    }
  }

}
