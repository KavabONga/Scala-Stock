package OrderExecutor

import ClientHandler.ClientHandler
import oper._
import com.typesafe.scalalogging.LazyLogging
import collection.mutable

class OrderExecutor(
                     var clients : mutable.Map[String, ClientHandler] = mutable.Map.empty,
                     var reqQueue : mutable.ListBuffer[QueuedOperation] = mutable.ListBuffer.empty
                   ) extends LazyLogging {
  private var allCurrencies = clients.values.flatMap(_.currencies.keys.toList).toList.distinct.to[mutable.Set]
  clients = clients.map(p => {
    val c = p._2
    c.filterCurrencies(allCurrencies)
    (p._1, c)
  })

  def getClient(name : String): ClientHandler = clients.getOrElse(name, throw new Exception(s"Client $name not found"))
  def removeClient(name : String) = {
    if (!clients.contains(name)) throw new Exception(s"Client $name not found")
    else {
      reqQueue = reqQueue.filterNot {
        case Selling(name_, _, _, _) => name_ == name
        case Purchase(name_, _, _, _) => name_ == name
        case _ => false
      }
      clients -= name
    }
  }
  def addClient(client : ClientHandler): Unit = {
    if (client.balance < 0) throw new Exception(s"${client.name}'s balance is negative")
    else {
      if (clients.contains(client.name)) throw new Exception(s"${client.name} is already in")
      else {
        allCurrencies ++= client.currencies.keys
        clients += client.name -> client.filterCurrencies(allCurrencies)
      }
    }
  }

  def getCurrencies: List[String] = allCurrencies.toList
  def getClientDict = clients
  def getClients: List[ClientHandler] = clients.values.toList
  def getRequestQueue = reqQueue

  def request(operation : QueuedOperation): Unit = {
    operation match {
      case Purchase(name, currency, count, price) => handlePurchase(name, currency, count, price)
      case Selling(name, currency, count, price) => handleSelling(name, currency, count, price)
    }
  }

  def updateClientBalance(client : String, balance : Double) =
    clients(client).updateBalance(balance)
  def addClientBalance(client : String, toAdd : Double) =
    clients(client).gainBalance(toAdd)

  def updateClientCurrency(client: String, currency : String, count : Int) =
    clients(client).updateCurrency(currency, count)
  def addClientCurrency(client : String, currency : String, toAdd : Int) =
    clients(client).gainCurrency(currency, toAdd)

  def addCurrency(currency : String, startCount : Int = 0): Unit = {
    if (allCurrencies.contains(currency)) throw new Exception(s"Currency $currency already involved")
    else {
      allCurrencies += currency
      clients = clients.map(p => p._1 -> p._2.addCurrency(currency, startCount))
    }
  }
  def removeCurrency(currency : String, price : Double) : Unit = {
    if (!allCurrencies.contains(currency)) throw new Exception(s"Currency $currency not found")
    else {
      allCurrencies -= currency
      clients = clients.map(p => p._1 -> p._2.removeCurrency(currency, price))
      reqQueue = reqQueue.filterNot({
        case c: Selling => c.currency == currency
        case c: Purchase => c.currency == currency
        case _ => false
      })
    }
  }

  def handlePurchase(name : String, currency : String, count : Int, price : Double):Unit = {
    if (!getClient(name).canSpend(count * price)) ()
    else {
      val sellRequests = reqQueue.filter((p : QueuedOperation) => {
        p match {
          case c : Selling => c.currency == currency && c.count <= count && c.price == price
          case _ => false
        }
      }) // List of opposite operations
      val sellPair = sellRequests.foldLeft((List[Selling](), 0))((res : (List[Selling], Int), op : QueuedOperation) => {
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
          case s : Selling => !sellPair._1.contains(s) && getClient(s.name).canSell(s.currency, s.count)
          case p : Purchase => p.name != name
          case _ => true
        }
      }) // Queue without fulfilled requests and requests leading to negative currency/dollar balance
      if (sellPair._1.isEmpty) {
        reqQueue += Purchase(name, currency, count, price)
      } // If noone who could sell the currency is found
      else {
        clients = clients.map(c => {
          val toSell = sellPair._1.filter(_.name == c._1).map(_.count).sum
          if (c._1 == name)
            c._1 -> c._2.buy(currency, sellPair._2, price)
          else
            c._1 -> c._2.sell(currency, toSell, price)
        })
        reqQueue = newQueue
      }
    }
  }
  def handleSelling(name: String, currency: String, count: Int, price: Double): Unit = {
    if (!getClient(name).canSell(currency, count)) ()
    else {
      val buyRequests = reqQueue.filter((p : QueuedOperation) => {
        p match {
          case c : Purchase => c.currency == currency && c.count <= count && c.price == price
          case _ => false
        }
      }) // List of opposite operations
      val buyPair = buyRequests.foldLeft((List[Purchase](), 0))((res : (List[Purchase], Int), op : QueuedOperation) => {
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
          case p : Purchase => !buyPair._1.contains(p) && getClient(p.name).canSpend(p.count * p.price)
          case s:Selling => s.name != name
          case _ => true
        }
      })
      if (buyPair._1.isEmpty) {
        reqQueue += Selling(name, currency, count, price)
      } // If noone who could sell the currency is found
      else {
        clients = clients.map(c => {
          val toBuy = buyPair._1.filter(p => p.name == c._1).map(_.count).sum
          if (c._1 == name)
            c._1 -> c._2.sell(currency, buyPair._2, price)
          else
            c._1 -> c._2.buy(currency, toBuy, price)
        })
        reqQueue = newQueue
      }
    }
  }
}
