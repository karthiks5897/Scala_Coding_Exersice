import scala.collection.mutable.ListBuffer
import scala.io.Source
import java.io.{File, PrintWriter}

// Define a case class to represent an order
case class Order(orderId: Int, userName: String, orderTime: Long, orderType: String, quantity: Int, price: Int)

// Define a case class to represent a match
case class Match(buyOrder: Order, sellOrder: Order, price: Int, quantity: Int, matchTime: Long)

object MatchingEngine {
  def main(args: Array[String]): Unit = {
    // Read orders from the CSV file
    val orders = Source.fromFile("exampleOrders.csv")
      .getLines()
      .drop(1) // Skip the header
      .map { line =>
        val fields = line.split(",")
        Order(fields(0).toInt, fields(1), fields(2).toLong, fields(3), fields(4).toInt, fields(5).toInt)
      }
      .toList

    // Sort orders by order time to process them in chronological order
    val sortedOrders = orders.sortBy(_.orderTime)

    // Initialize order books for BUY and SELL orders
    val buyOrderBook = ListBuffer[Order]()
    val sellOrderBook = ListBuffer[Order]()
    val matches = ListBuffer[Match]()

    for (order <- sortedOrders) {
      order.orderType match {
        case "BUY" =>
          // Try to match BUY orders with SELL orders
          val matchingSellOrders = sellOrderBook.filter(sellOrder => sellOrder.quantity == order.quantity)
          if (matchingSellOrders.nonEmpty) {
            val matchedSellOrder = matchingSellOrders.head
            val matchPrice = matchedSellOrder.price
            val matchTime = System.currentTimeMillis()
            matches += Match(order, matchedSellOrder, matchPrice, order.quantity, matchTime)
            sellOrderBook -= matchedSellOrder
          } else {
            // No matching SELL order found, add the BUY order to the book
            buyOrderBook += order
          }

        case "SELL" =>
          // Try to match SELL orders with BUY orders
          val matchingBuyOrders = buyOrderBook.filter(buyOrder => buyOrder.quantity == order.quantity)
          if (matchingBuyOrders.nonEmpty) {
            val matchedBuyOrder = matchingBuyOrders.head
            val matchPrice = matchedBuyOrder.price
            val matchTime = System.currentTimeMillis()
            matches += Match(matchedBuyOrder, order, matchPrice, order.quantity, matchTime)
            buyOrderBook -= matchedBuyOrder
          } else {
            // No matching BUY order found, add the SELL order to the book
            sellOrderBook += order
          }
      }
    }

    // Write match details to an output CSV file
    val outputCSV = new PrintWriter(new File("outputExampleMatches.csv"))
    outputCSV.println("Buy Order ID, Sell Order ID, Price, Quantity, Match Time")
    matches.foreach { matchInfo =>
      outputCSV.println(s"${matchInfo.buyOrder.orderId}, ${matchInfo.sellOrder.orderId}, ${matchInfo.price}, ${matchInfo.quantity}, ${matchInfo.matchTime}")
    }
    outputCSV.close()
  }
}
