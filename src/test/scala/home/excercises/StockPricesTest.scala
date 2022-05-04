package home.excercises

import org.scalatest.matchers.should.Matchers

object StockPricesTest extends App with Matchers {

  case class Solution(buy: Int, sell: Int)

  def calculate(stockPrices: List[Int]): Option[Solution] = {

    stockPrices match {
      case ps if ps.size < 2 ⇒ None
      case p1 +: p2 +: ps ⇒
        val (buy, profit) = ps.foldLeft((p1, p2 - p1)) {
          case ((minPriceSoFar, maxProfitSoFar), price) ⇒
            val newMaxProfit = Math.max(maxProfitSoFar, price - minPriceSoFar)
            val newMin = Math.min(price, minPriceSoFar)
            (newMin, newMaxProfit)
        }
        Some(Solution(buy, buy + profit))
    }
  }

  calculate(List.empty) shouldBe None
  calculate(List(10)) shouldBe None
  calculate(List(10, 7)) shouldBe Some(Solution(10, 7))
  calculate(List(10, 7, 5, 8, 11, 9)) shouldBe Some(Solution(5, 11))

}
