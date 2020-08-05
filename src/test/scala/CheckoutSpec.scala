import java.util.UUID

import Checkout._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CheckoutSpec extends AnyFreeSpec with Matchers {

  "calculateTotalValueOfBasket" - {

    "calculateTotalValueOfBasket should return value of 60" in {
      val dateAndTime = java.time.LocalDateTime.now()
      val basket = List(
        Product('C', 20, None, dateAndTime),
        Product('C', 20, None, dateAndTime),
        Product('C', 20, None, dateAndTime)
      )

      calculateTotalValueOfBasket(basket) shouldBe 60
    }

    "calculateTotalValueOfBasket should return value of 130" in {
      val dateAndTime = java.time.LocalDateTime.now()
      val basket = List(
        Product('A', 50, Some(Offer(3, 130)), dateAndTime),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime)
      )

      calculateTotalValueOfBasket(basket) shouldBe 130
    }

    "calculateTotalValueOfBasket should return value of 180" in {
      val dateAndTime = java.time.LocalDateTime.now()
      val basket = List(
        Product('A', 50, Some(Offer(3, 130)), dateAndTime),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime)
      )

      calculateTotalValueOfBasket(basket) shouldBe 180
    }

    "calculateTotalValueOfBasket should handle different order of products and should return value of 285" in {
      val dateAndTime = java.time.LocalDateTime.now()
      val basket = List(
        Product('A', 50, Some(Offer(3, 130)), dateAndTime),
        Product('B', 30, Some(Offer(2, 45)), dateAndTime),
        Product('C', 20, None, dateAndTime),
        Product('B', 30, Some(Offer(2, 45)), dateAndTime),
        Product('C', 20, None, dateAndTime),
        Product('C', 20, None, dateAndTime),
        Product('D', 15, None, dateAndTime),
        Product('C', 20, None, dateAndTime),
        Product('D', 15, None, dateAndTime),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime)
      )

      calculateTotalValueOfBasket(basket) shouldBe 285
    }

    "calculateTotalValueOfBasket should return value of 0" in {
      val basket = List()

      calculateTotalValueOfBasket(basket) shouldBe 0
    }
  }

  "calculateNumberOfOccurances" - {

    "calculateNumberOfOccurances should return a Map containing Char -> Int representing number of each item in basket" in {
      val dateAndTime = java.time.LocalDateTime.now()
      val basket = List(
        Product('A', 50, Some(Offer(3, 130)), dateAndTime),
        Product('B', 30, Some(Offer(2, 45)), dateAndTime),
        Product('C', 20, None, dateAndTime),
        Product('B', 30, Some(Offer(2, 45)), dateAndTime),
        Product('C', 20, None, dateAndTime),
        Product('C', 20, None, dateAndTime),
        Product('D', 15, None, dateAndTime),
        Product('C', 20, None, dateAndTime),
        Product('D', 15, None, dateAndTime),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime)
      )

      calculateNumberOfOccurances(basket) shouldBe Map('D' -> 2, 'A' -> 3, 'C' -> 4, 'B' -> 2)
    }

    "calculateNumberOfOccurances return a Empty Map where basket is empty" in {
      val basket = List()

      calculateNumberOfOccurances(basket) shouldBe Map()
    }
  }

  "calculateCostForDiscountedItem" - {
    "Should return total prices for Product in relation to how many of same Product are in the basket" in {
      val dateAndTime = java.time.LocalDateTime.now()

      calculateCostForDiscountedItem(Product('A', 50, Some(Offer(3, 130)), dateAndTime), 0) shouldBe 0

      calculateCostForDiscountedItem(Product('A', 50, Some(Offer(3, 130)), dateAndTime), 4) shouldBe 180

      calculateCostForDiscountedItem(Product('B', 30, Some(Offer(2, 45)), dateAndTime), 2) shouldBe 45

      calculateCostForDiscountedItem(Product('C', 20, None, dateAndTime), 10) shouldBe 200

      calculateCostForDiscountedItem(Product('D', 15, None, dateAndTime), 10) shouldBe 150
    }
  }

  "get top ten best selling items within 24 hours" - {
    "Should return top 10 best selling items within the last 24hours" in {
      val dateAndTime = java.time.LocalDateTime.now()
      val itemsA = List(
        Product('A', 50, Some(Offer(3, 130)), dateAndTime.minusHours(25)),
        Product('C', 20, None, dateAndTime.minusHours(10)),
        Product('C', 20, None, dateAndTime.minusHours(1)),
        Product('C', 20, None, dateAndTime.minusHours(22)),
        Product('D', 15, None, dateAndTime.minusHours(22)),
        Product('D', 15, None, dateAndTime.minusHours(22)),
        Product('D', 15, None, dateAndTime.minusHours(22)),
        Product('D', 15, None, dateAndTime.minusHours(22)),
        Product('D', 15, None, dateAndTime.minusHours(22)),
        Product('B', 30, Some(Offer(2, 45)), dateAndTime.minusHours(22)),
        Product('B', 30, Some(Offer(2, 45)), dateAndTime),
        Product('B', 30, Some(Offer(2, 45)), dateAndTime.minusHours(100)),
        Product('B', 30, Some(Offer(2, 45)), dateAndTime)
      )
      val itemsB = List(
        Product('A', 50, Some(Offer(3, 130)), dateAndTime.minusHours(54)),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime.minusHours(30)),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime.minusHours(6)),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime.minusHours(3)),
        Product('B', 50, Some(Offer(3, 130)), dateAndTime),
        Product('B', 50, Some(Offer(3, 130)), dateAndTime),
        Product('D', 15, None, dateAndTime.minusHours(22)),
        Product('D', 15, None, dateAndTime.minusHours(2)),
        Product('D', 15, None, dateAndTime.minusHours(7)),
        Product('C', 20, None, dateAndTime.minusHours(2)),
        Product('C', 20, None, dateAndTime.minusHours(22)),
        Product('C', 20, None, dateAndTime.minusHours(1)),
        Product('C', 20, None, dateAndTime.minusHours(1))
      )
      val totalSales = List(
        Basket(UUID.randomUUID(), itemsA),
        Basket(UUID.randomUUID(), itemsB),
        Basket(UUID.randomUUID(), itemsB)
      )

      TopTenBestSellingItemsWithin24Hours(totalSales) shouldBe List('D', 'C', 'B', 'A')
    }
    "Should only return 10 or less" in {
      val dateAndTime = java.time.LocalDateTime.now()
      val itemsA = List(
        Product('A', 50, Some(Offer(3, 130)), dateAndTime.minusHours(25)),
        Product('B', 30, Some(Offer(2, 45)), dateAndTime.minusHours(22)),
        Product('C', 20, None, dateAndTime.minusHours(10)),
        Product('D', 15, None, dateAndTime.minusHours(10)),
        Product('E', 10, None, dateAndTime.minusHours(10)),
        Product('F', 5, None, dateAndTime.minusHours(10)),
        Product('G', 1, None, dateAndTime.minusHours(10))
      )
      val itemsB = List(
        Product('A', 50, Some(Offer(3, 130)), dateAndTime.minusHours(25)),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime.minusHours(25)),
        Product('D', 15, None, dateAndTime.minusHours(10)),
        Product('E', 10, None, dateAndTime.minusHours(50)),
        Product('F', 5, None, dateAndTime.minusHours(25)),
        Product('I', 5, None, dateAndTime.minusHours(3)),
        Product('I', 5, None, dateAndTime.minusHours(3)),
        Product('I', 5, None, dateAndTime.minusHours(3)),
        Product('I', 5, None, dateAndTime.minusHours(3)),
        Product('Z', 5, None, dateAndTime.minusHours(3)),
        Product('Z', 5, None, dateAndTime.minusHours(3)),
        Product('Z', 5, None, dateAndTime.minusHours(3)),
        Product('I', 5, None, dateAndTime.minusHours(3)),
        Product('I', 5, None, dateAndTime.minusHours(3)),
        Product('I', 1, None, dateAndTime.minusHours(3))
      )

      val itemsC = List(
        Product('A', 50, Some(Offer(3, 130)), dateAndTime.minusHours(2)),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime.minusHours(2)),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime.minusHours(3)),
        Product('D', 15, None, dateAndTime.minusHours(40)),
        Product('E', 10, None, dateAndTime.minusHours(5)),
        Product('F', 5, None, dateAndTime.minusHours(25)),
        Product('G', 1, None, dateAndTime.minusHours(100)),
        Product('H', 1, None, dateAndTime.minusHours(1)),
        Product('H', 1, None, dateAndTime.minusHours(1)),
        Product('H', 1, None, dateAndTime.minusHours(1)),
        Product('H', 1, None, dateAndTime.minusHours(1))
      )
      val totalSales = List(
        Basket(UUID.randomUUID(), itemsA),
        Basket(UUID.randomUUID(), itemsB),
        Basket(UUID.randomUUID(), itemsC)
      )

      TopTenBestSellingItemsWithin24Hours(totalSales).length shouldBe 10
    }

    "Should return empty List" in {
      val dateAndTime = java.time.LocalDateTime.now()
      val itemsA = List.empty
      val itemsB = List.empty
      val itemsC = List(
        Product('A', 50, Some(Offer(3, 130)), dateAndTime.minusHours(99)),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime.minusHours(25)),
        Product('A', 50, Some(Offer(3, 130)), dateAndTime.minusHours(26)),
        Product('D', 15, None, dateAndTime.minusHours(34)),
        Product('E', 10, None, dateAndTime.minusHours(55)),
        Product('F', 5, None, dateAndTime.minusHours(25)),
        Product('G', 1, None, dateAndTime.minusHours(100)),
        Product('H', 1, None, dateAndTime.minusHours(51)),
        Product('H', 1, None, dateAndTime.minusHours(61)),
        Product('H', 1, None, dateAndTime.minusHours(71)),
        Product('H', 1, None, dateAndTime.minusHours(91))
      )
      val totalSales = List(
        Basket(UUID.randomUUID(), itemsA),
        Basket(UUID.randomUUID(), itemsB),
        Basket(UUID.randomUUID(), itemsC)
      )

      TopTenBestSellingItemsWithin24Hours(totalSales) shouldBe List.empty
    }

  }

}