package home.datastructures

import home.datastructures.BinaryTree._
import org.scalatest.{FlatSpec, Matchers}
import scala.collection.immutable._

class BinaryTreeTest extends FlatSpec with Matchers {

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

}
