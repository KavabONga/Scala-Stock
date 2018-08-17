package OrderExecutor

import ClientHandler.ClientHandler
import Operation._
import com.typesafe.scalalogging.LazyLogging

class OrderExecutor(clients_ : Map[String, ClientHandler], reqQueue : List[Operation]) extends LazyLogging {
  private val allCurrencies = clients_.values.flatMap(_.currencies.keys.toList).toList.distinct
  private val clients: Map[String, ClientHandler] = clients_.map(p => {
    val newCurs = allCurrencies.filterNot(c => p._2.currencies.contains(c))
    (p._1, new ClientHandler(p._2.name, p._2.balance, p._2.currencies ++ newCurs.map(t => t -> 0).toMap))
  })
  def this (clients_ : List[ClientHandler], reqQueue_ : List[Operation] = Nil) =
    this(clients_.map(c => c.name -> c).toMap, reqQueue_)

  def getClient(name : String): ClientHandler = clients.getOrElse(name, throw new Exception)
  def getClientDict: Map[String, ClientHandler] = clients
  def getClients: List[ClientHandler] = clients.values.toList
  def removeClient(name : String): OrderExecutor =
    new OrderExecutor(clients - name, reqQueue.filterNot {
      case Selling(name_, _, _, _) => name_ == name
      case Purchase(name_, _, _, _) => name_ == name
      case _ => false
    })
  def addClient(client : ClientHandler): OrderExecutor = {
    val newCurs = allCurrencies.filterNot(c => client.currencies.contains(c))
    new OrderExecutor(new ClientHandler(client.name, client.balance, client.currencies ++ newCurs.map(t => t -> 0).toMap) :: getClients, reqQueue)
  }

  def getCurrencies: List[String] = allCurrencies

  def request(operation : Operation): OrderExecutor = {
    operation match {
      case Purchase(name, currency, count, price) => handlePurchase(name, currency, count, price)
      case Selling(name, currency, count, price) => handleSelling(name, currency, count, price)
      case CurrencyAddition(currency) => addCurrency(currency)
      case CurrencyRemoval(currency, price) => removeCurrency(currency, price)
      case ClientAddition(name, balance) => addClient(new ClientHandler(name, balance))
      case ClientRemoval(name) => removeClient(name)
      case EmptyOperation() => this
    }
  }

  def addCurrency(currency : String, startCount : Int = 0): OrderExecutor = {
    new OrderExecutor(
      clients.map(c => c._1 -> c._2.addCurrency(currency, startCount)),
      reqQueue
    )
  }
  def removeCurrency(currency : String, price : Double) : OrderExecutor = {
    new OrderExecutor(
      clients.map(c => c._1 -> c._2.removeCurrency(currency, price)),
      reqQueue
    )
  }

  def handlePurchase(name : String, currency : String, count : Int, price : Double):OrderExecutor = {
    if (!getClient(name).canSpend(count * price)) {
      logger.debug(s"Rejected ${name}s Purchase")
      this
    }
    else {
      val sellRequests = reqQueue.filter((p : Operation) => {
        p match {
          case c : Selling => c.currency == currency && c.count <= count && c.price == price
          case _ => false
        }
      }) // List of opposite operations
      val sellPair = sellRequests.foldLeft((List[Selling](), 0))((res : (List[Selling], Int), op : Operation) => {
        op match {
          case p : Selling =>
            if (res._2 + p.count <= count && p.name != name && getClient(p.name).canSell(p.currency, p.count))
              (res._1 :+ p, res._2 + p.count)
            else
              res
          case _ => throw new Exception
        }
      }) // Pair of (List of proper Selling operations, summary count)
      val newQueue = reqQueue.filter(op => {
        op match {
          case p : Selling => sellPair._1.contains(p) || getClient(p.name).canSell(p.currency, p.count)
          case _ => false
        }
      }) // Queue without fulfilled requests and requests leading to negative currency/dollar balance
      if (sellPair._1.isEmpty) {
        new OrderExecutor(
          clients,
          reqQueue :+ Purchase(name, currency, count, price)
        )
      } // If noone who could sell the currency is found
      else {
        val newClients = clients.map(c => {
          val toSell = sellPair._1.filter(_.name == c._1).map(_.count).sum
          if (c._1 == name)
            c._2.buy(currency, sellPair._2, price)
          else
            c._2.sell(currency, toSell, price)
        }).toList
        new OrderExecutor(newClients, newQueue)
      }
    }
  }
  def handleSelling(name: String, currency: String, count: Int, price: Double): OrderExecutor = {
    if (!getClient(name).canSell(currency, count)) {
      logger.debug(s"Rejected ${name}s Selling")
      this
    }
    else {
      val buyRequests = reqQueue.filter((p : Operation) => {
        p match {
          case c : Purchase => c.currency == currency && c.count <= count && c.price == price
          case _ => false
        }
      }) // List of opposite operations
      val buyPair = buyRequests.foldLeft((List[Purchase](), 0))((res : (List[Purchase], Int), op : Operation) => {
        op match {
          case p : Purchase =>
            if (res._2 + p.count <= count && p.name != name && getClient(p.name).canSpend(p.count * p.price))
              (res._1 :+ p, res._2 + p.count)
            else
              res
          case _ => throw new Exception
        }
      }) // Returns a pair of (List of proper Purchase, summary count)
      val newQueue = reqQueue.filter(op => {
        op match {
          case p : Purchase => buyPair._1.contains(p) || getClient(p.name).canSpend(p.count * p.price)
          case _ => false
        }
      })
      if (buyPair._1.isEmpty) {
        new OrderExecutor(
          clients,
          reqQueue :+ Selling(name, currency, count, price)
        )
      } // If noone who could sell the currency is found
      else {
        val newClients = clients.map(c => {
          val toBuy = buyPair._1.filter(p => p.name == c._1).map(_.count).sum
          if (c._1 == name)
            c._2.sell(currency, buyPair._2, price)
          else
            c._2.buy(currency, toBuy, price)
        }).toList
        new OrderExecutor(newClients, newQueue)
      }
    }
  }
  def getRequestQueue(): List[Operation] = reqQueue
}
