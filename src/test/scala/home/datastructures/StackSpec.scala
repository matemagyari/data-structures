package home.datastructures

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StackSpec extends AnyFlatSpec with Matchers with OptionValues {

  "Stack" should "work" in {
    class MinStack() {

      val numsOccurances = scala.collection.mutable.Map.empty[Int, Int]

      var elems = List.empty[Int]

      def push(`val`: Int) {
        elems = `val` +: elems
        numsOccurances += `val` -> (numsOccurances.getOrElse(`val`, 0) + 1)
      }

      def pop() {
        val elem = elems.head
        elems = elems.tail
        val occurrence = numsOccurances.get(elem).map(_ - 1).getOrElse(0)
        if (occurrence == 0) {
          numsOccurances -= elem
        }
        else {
          numsOccurances += elem -> occurrence
        }
      }

      def top(): Int = {
        elems.head
      }

      def getMin(): Int = {
        numsOccurances.keys.min
      }
    }

    val stack = new MinStack()
    stack.push(-2)
    stack.push(0)
    stack.push(-3)

    stack.getMin() shouldBe -3
    stack.pop()
    stack.top() shouldBe 0
    stack.getMin() shouldBe -2


    //      ["MinStack","push","push","push","getMin","pop","top","getMin"]
    //      [[],[-2],[0],[-3],[],[],[],[]]
  }

}
