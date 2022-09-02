package home.datastructures

import home.datastructures.BinaryTree._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.immutable._

class BinaryTreeSpec extends AnyFlatSpec with Matchers {

  //  tree
  //  ----
  //            j    <-- root
  //          /   \
  //        f      k
  //      /   \      \
  //    a     h      z
  //     \
  //      d

  val tree: Tree[Char] = Tree(
    value = 'j',
    left = Some(Tree('f', left = Some(Tree('a', right = Some(Tree('d')))), right = Some(Tree('h')))),
    right = Some(Tree('k', right = Some(Tree('z'))))
  )

  "Breadth-first traverse of binary tree " should "work" in {
    val expected = Seq('j', 'f', 'k', 'a', 'h', 'z', 'd')
    traverseBreadthFirst(tree) shouldBe expected
    traverseBreadthFirstWithQueue(tree) shouldBe expected
  }

  "Preorder depth-first traverse of binary tree " should "work" in {
    traversePreorderDepthFirst(tree) shouldBe Seq('j', 'f', 'a', 'd', 'h', 'k', 'z')
  }

  "Inorder depth-first traverse of binary tree " should "work" in {
    traverseInOrderDepthFirst(tree) shouldBe Seq('a', 'd', 'f', 'h', 'j', 'k', 'z')
  }

  "Postorder depth-first traverse of binary tree " should "work" in {
    traversePostOrderDepthFirst(tree) shouldBe Seq('d', 'a', 'h', 'f', 'z', 'k', 'j')
  }


  "Max depth" should "work" in {
    class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
      var value: Int = _value
      var left: TreeNode = _left
      var right: TreeNode = _right
    }
    object Solution {
      def maxDepth(root: TreeNode): Int = {
        if (root == null) 0
        else 1 + Math.max(maxDepth(root.left), maxDepth(root.right))
      }
    }
  }

  "Is valid binary search tree" should "work" in {
    class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
      var value: Int = _value
      var left: TreeNode = _left
      var right: TreeNode = _right
    }

    def helper(root: TreeNode, low: Integer, right: Integer): Boolean =
      root == null || {
        (low == null || root.value > low) &&
          (right == null || root.value < right) &&
          helper(root.left, low, root.value) &&
          helper(root.right, root.value, right)
      }

    def isValidBST(root: TreeNode): Boolean = {
      helper(root, null, null)
    }

    val r = new TreeNode(-2147483648)
    r.right = new TreeNode(2147483647)

    isValidBST(r) shouldBe true
  }

  "Is symmetric binary tree" should "work" in {
    class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
      var value: Int = _value
      var left: TreeNode = _left
      var right: TreeNode = _right
    }

    def infix(root: TreeNode, left: Boolean = true): List[(Int, Boolean)] =
      if (root == null) List.empty[(Int, Boolean)]
      else
        infix(root.left, true) ++ List((root.value, left)) ++ infix(root.right, false)

    def isSymmetric(root: TreeNode): Boolean = {
      if (root == null) true
      else {
        val left = infix(root.left, left = true)
        val right = infix(root.right, left = false)

        if (left.size != right.size) false
        else {
          (0 to left.size - 1).forall { index =>
            val l = left(index)
            val r = right(left.size - index - 1)
            l._1 == r._1 && l._2 != r._2
          }

        }
      }
    }
  }

  "Level order traversal" should "work" in {
    class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
      var value: Int = _value
      var left: TreeNode = _left
      var right: TreeNode = _right
    }

    case class Node(value: Int, level: Int)
    def helper(root: TreeNode, level: Int): List[Node] = {
      if (root == null) List.empty
      else helper(root.left, level + 1) ++ List(Node(root.value, level)) ++ helper(root.right, level + 1)

    }

    def levelOrder(root: TreeNode): List[List[Int]] = {
      val valuesWithLevels: List[Node] = helper(root, 0)
      valuesWithLevels.groupBy(_.level).toList.sortBy(_._1).map(_._2.map(_.value))
    }
  }

  "Convert Sorted Array to Binary Search Tree" should "work" in {
    class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
      var value: Int = _value
      var left: TreeNode = _left
      var right: TreeNode = _right
    }


    def sortedArrayToBST(nums: Array[Int]): TreeNode =
      if (nums.size == 0) null
      else {
        val midIndex = nums.size / 2
        val leftNums = nums.take(midIndex)
        val rightNums = nums.drop(midIndex + 1)
        new TreeNode(
          _value = nums(midIndex),
          _left = sortedArrayToBST(leftNums),
          _right = sortedArrayToBST(rightNums))
      }
  }

  "inorderTraversal" should "work" in {
    class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
      var value: Int = _value
      var left: TreeNode = _left
      var right: TreeNode = _right
    }

    def inorderTraversal(root: TreeNode): List[Int] =
      if (root == null) List.empty
      else inorderTraversal(root.left) ++ List(root.value) ++ inorderTraversal(root.right)
  }

  "inorderTraversal" should "work 2" in {
    class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
      var value: Int = _value
      var left: TreeNode = _left
      var right: TreeNode = _right
    }

    def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {

      type Level = Int

      def inorderTraversal(root: TreeNode, level: Level): List[(Level, Int)] =
        if (root == null) List.empty
        else inorderTraversal(root.left, level + 1) ++
          List((level, root.value)) ++
          inorderTraversal(root.right, level + 1)

      inorderTraversal(root, 1)
        .groupBy(_._1)
        .toList
        .sortBy(_._1)
        .map { x =>
          val values = x._2.map(_._2)
          if (x._1 % 2 == 1) values else values.reverse
        }
    }
  }

  "Serialization with preorder traversal " should "work" in {
    class TreeNode(var _value: Int) {
      var value: Int = _value
      var left: TreeNode = null
      var right: TreeNode = null
    }

    class Codec {
      // Encodes a list of strings to a single string.
      def serialize(root: TreeNode): String = {
        def sserialize(root: TreeNode): List[String] = {
          if (root == null) List("n")
          else
            List(root.value.toString) ++ sserialize(root.left) ++ sserialize(root.right)
        }
        sserialize(root).mkString(",")
      }

      // Decodes a single string to a list of strings.
      def deserialize(data: String): TreeNode = {
        val chars = data.split(",")
        var idx = 0
        def dfs(): TreeNode =
          if (chars(idx) == "n") {
            idx += 1
            null
          }
          else {
            val t =  new TreeNode(chars(idx).toInt)
            idx += 1
            t.left = dfs()
            t.right = dfs()
            t
          }

        dfs()
      }
    }

    val codec = new Codec

    {
      val root = new TreeNode(1)
      root.left = new TreeNode(2)
      val tree3 = new TreeNode(3)
      tree3.left = new TreeNode(4)
      tree3.right = new TreeNode(5)
      root.right = tree3

      codec.serialize(root) shouldBe List("1", "2", "n", "n", "3", "4", "n", "n", "5", "n", "n")
    }

    val tree = new TreeNode(8)
    tree.left = new TreeNode(3)
    tree.right = new TreeNode(11)

    codec.serialize(tree) shouldBe List("8", "3", "n", "n", "11", "n", "n")

    val result = codec.deserialize(List("1", "2", "n", "n", "3", "4", "n", "n", "5", "n", "n").mkString(","))
    result shouldBe 1

  }


}
