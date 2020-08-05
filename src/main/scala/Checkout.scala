import java.time.LocalDateTime
import java.util.UUID

object Checkout extends App {
  val dateAndTime = java.time.LocalDateTime.now()

  case class Product(productId: Char, unitPrice: Int, offer: Option[Offer], purchasedAt: LocalDateTime)
  case class Offer(numberNeededForDiscount: Int, batchPrice: Int)
  case class Basket(basketId: UUID, items: List[Product])

  def calculateNumberOfOccurances(products: List[Product]): Map[Char, Int] =
    products
      .groupBy(_.productId)
      .map { case (pid, instances) => (pid, instances.length) }
      .withDefaultValue(0)

  def calculateCostForDiscountedItem(product: Product,
                                     numberOfItems: Int): Int =
    product.offer match {
      case None => product.unitPrice * numberOfItems
      case Some(offer) =>
        val excludedFromDiscount = numberOfItems % offer.numberNeededForDiscount
        val discountBatches = numberOfItems / offer.numberNeededForDiscount
        (excludedFromDiscount * product.unitPrice) + (discountBatches * offer.batchPrice)
    }

  def calculateTotalValueOfBasket(products: List[Product]): Int = {
    val itemOccurances: Map[Char, Int] =
      calculateNumberOfOccurances(products)

    products.distinct.map { product =>
      calculateCostForDiscountedItem(product, itemOccurances(product.productId))
    }.sum
  }

  def isDateWithin24Hours(purchaseDate: LocalDateTime): Boolean = {
    val now = LocalDateTime.now
    purchaseDate.isBefore(now.plusDays(1)) && purchaseDate.isAfter(now.minusDays(1))
  }

  def TopTenBestSellingItemsWithin24Hours(totalSales: List[Basket]): List[Char] = {
    val productsSoldWithin24Hours = for {
      items        <- totalSales.map(_.items)
      productsSold <- items.filter(x => isDateWithin24Hours(x.purchasedAt))
    } yield productsSold

    calculateNumberOfOccurances(productsSoldWithin24Hours).toList.sortBy(_._2).reverse.take(10).map(_._1)
  }

  val itemsA = List(
    Product('A', 50, Some(Offer(3, 130)), dateAndTime.minusHours(25)),
    Product('C', 20, None, dateAndTime.minusHours(22)),
    Product('B', 30, Some(Offer(2, 45)), dateAndTime)
  )

  val itemsB = List(
    Product('A', 50, Some(Offer(3, 130)), dateAndTime.minusHours(30)),
    Product('B', 50, Some(Offer(3, 130)), dateAndTime)
  )

  val totalSales = List(
    Basket(UUID.randomUUID(), itemsA),
    Basket(UUID.randomUUID(), itemsB)
  )

  // Calculate total of basket including discounts
  println(
    s"${Console.YELLOW}Total price:${Console.GREEN} ${calculateTotalValueOfBasket(itemsA)}${Console.RESET}"
  )

  // Calculate the top ten best selling items within the last 24 hours.
  println(
    s"${Console.YELLOW}Top selling items in the last 24 hours are:${Console.GREEN} ${TopTenBestSellingItemsWithin24Hours(totalSales)}${Console.RESET}"
  )

}