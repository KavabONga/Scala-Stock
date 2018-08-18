package serializer

import ClientHandler.ClientHandler
import Operation._
import org.json4s.JsonAST.JValue
import org.json4s.jackson.JsonMethods
import org.json4s.JsonDSL
import org.json4s.jackson.{Json, JsonMethods}
import org.json4s.jackson.JsonMethods.{compact, render, pretty}
import org.json4s.JsonDSL._

object ObjectsToJson {
  def error(exc : Throwable) =
    pretty(render(("successful" -> false) ~ ("error" -> exc.getMessage)))
  def success =
    pretty(render("success" -> true))
  private def packClient(client : ClientHandler) = {
    val currenciesStr = client.currencies.mapValues(_.toString).toList
    ("name" -> client.name)~
      ( "balance" -> client.balance.toString)~
      ("currencies" -> currenciesStr)
    
  }
  def clientToJson(client : ClientHandler) = pretty(render(packClient(client)))
  def clientsToJson(clients : List[ClientHandler]) = pretty(render(clients.map(packClient)))
  private def sellBuyPack(name : String, cur : String, count : Int, price : Double) =
    ("name"->name)~("currency"->cur)~("count"->count)~("price"->price)
  private def operationPack(oper : Operation):JValue =
    oper match {
      case Selling(name, cur, count, price) =>
        ("type" -> "Selling") ~ ( "parameters" -> sellBuyPack(name, cur, count, price))
      case Purchase(name, cur, count, price) =>
        ("type" -> "Selling") ~ ( "parameters" -> sellBuyPack(name, cur, count, price))
      case _ =>
        "type" -> "error"
    }
  def operationToJson(oper : Operation) :String = pretty(render(operationPack(oper)))
  def operationsToJson(opers : List[Operation]) : String = pretty(render(opers.map(operationPack)))
}
