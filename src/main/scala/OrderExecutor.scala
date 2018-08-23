package OrderExecutor

import ClientHandler.ClientHandler
import oper._
import com.typesafe.scalalogging.LazyLogging

import collection.mutable
import scala.collection.mutable.ListBuffer

case class MemoryPair(toPush : QueuedOperation, alt : QueuedOperation)

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

  def this(cl : Iterable[ClientHandler], reqs : Iterable[QueuedOperation]) =
    this(
      mutable.Map.empty ++ cl.map(c => c.name -> c).toMap,
      reqs.to[mutable.ListBuffer]
    )

  private var memory = Option.empty[MemoryPair]
  private def memorize(toPush : QueuedOperation, alt : QueuedOperation) =
    memory = Some(MemoryPair(toPush, alt))
  private def dropMemory() = {
    if (memory.isDefined) {
      reqQueue += memory.get.toPush
      memory = Option.empty[MemoryPair]
    }
    else ()
  }
  private def useMemory() = {
    val operFirst = memory.get.toPush
    val operAlt = memory.get.alt
    reqQueue = reqQueue.foldLeft((List[QueuedOperation](), false)) { (p, op) =>
      if (op == memory.get.alt && !p._2) (p._1, true)
      else (p._1 :+ op, p._2)
    }._1.to[ListBuffer]
    operFirst match {
      case c : Selling => {
        clients(operFirst.name).sell(operFirst.currency, operFirst.count, operAlt.price)
        clients(operAlt.name).buy(operAlt.currency, operAlt.count, operAlt.price)
      }
      case c : Purchase => {
        clients(operFirst.name).buy(operFirst.currency, operFirst.count, operAlt.price)
        clients(operAlt.name).sell(operAlt.currency, operAlt.count, operAlt.price)
      }
    }
    ()
  }
  def justLowered: Boolean =
    memory.isDefined

  def acceptOperationLowering() = {
    if (memory.isDefined)
      useMemory()
    else throw new Exception("Nothing to verify")
  }

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
        clients = mutable.Map.empty ++ clients.mapValues(c => c.filterCurrencies(allCurrencies))
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

  private def oppositeQueuedOperations(op : QueuedOperation) = {
    op match {
      case Selling(name, currency, count, price) => {
        reqQueue.filter((p : QueuedOperation) => {
          p match {
            case c : Purchase => c.currency == currency && c.price == price && c.name != name
            case _ => false
          }
        })
      }
      case Purchase(name, currency, count, price) => {
        reqQueue.filter((p : QueuedOperation) => {
          p match {
            case c : Selling => c.currency == currency && c.price == price && c.name != name
            case _ => false
          }
        })
      }
    }
  }
  private def singlePossibleOpposite(op : QueuedOperation) = {
    op match {
      case p : Selling => {
        val pos = reqQueue.filter(posOp => {
          posOp match {
            case posP : Purchase => posP.count == op.count && posP.currency == op.currency && posP.price <= p.price
            case _ => false
          }
        })
        if (pos.isEmpty)
          Option.empty[QueuedOperation]
        else
          Option(pos.head)
      }
      case p : Purchase => {
        val pos = reqQueue.filter(posOp => {
          posOp match {
            case posP : Selling => posP.count == op.count && posP.currency == op.currency && posP.price >= p.price
            case _ => false
          }
        })
        if (pos.isEmpty)
          Option.empty[QueuedOperation]
        else
          Option(pos.head)
      }
    }
  }

  def handlePurchase(name : String, currency : String, count : Int, price : Double):Unit = {
    dropMemory()
    if (!getClient(name).canSpend(count * price)) ()
    else {
      val toPush = Purchase(name, currency, count, price)
      val sellRequests = oppositeQueuedOperations(toPush) // List of opposite operations
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
        val pos = singlePossibleOpposite(toPush)
        if (pos.isEmpty)
          reqQueue += toPush
        else {
          memorize(toPush, pos.get)
        }
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
    dropMemory()
    if (!getClient(name).canSell(currency, count)) ()
    else {
      val toPush = Selling(name, currency, count, price)
      val buyRequests = oppositeQueuedOperations(toPush) // List of opposite operations
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
        val pos = singlePossibleOpposite(toPush)
        if (pos.isEmpty)
          reqQueue += toPush
        else
          memorize(toPush, pos.get)
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
