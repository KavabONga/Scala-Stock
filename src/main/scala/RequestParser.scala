import ClientHandler.ClientHandler
import Operation._

import scala.util.Try

object RequestParser {
  def tryToClient(m : Map[String, String]) : Try[ClientHandler] =
    Try({
      new ClientHandler(
        m("name"),
        m.get("balance").map(_.toDouble).getOrElse(0.0),
        (m - "name" - "balance").mapValues(_.toInt)
      )
    })
  def tryToSelling(m : Map[String, String]) : Try[Selling] = {
    Try({
      Selling(
        m("name"),
        m("currency"),
        m.get("count").map(_.toInt).get,
        m.get("price").map(_.toDouble).get
      )
    })
  }
  def tryToPurchase(m : Map[String, String]) : Try[Purchase] = {
    Try({
      Purchase(
        m("name"),
        m("currency"),
        m.get("count").map(_.toInt).get,
        m.get("price").map(_.toDouble).get
      )
    })
  }
}