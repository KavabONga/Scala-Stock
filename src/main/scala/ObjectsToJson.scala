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
  def error(exc : Exception) =
    pretty(render(("successful" -> false) ~ ("error" -> exc.getMessage)))
  def success =
    pretty(render("success" -> true))
  def clientToJson(client : ClientHandler): String = {
    val currenciesStr = client.currencies.mapValues(_.toString).toList
    val serialized =
      ("name" -> client.name)~
      ( "balance" -> client.balance.toString)~
        ("currencies" -> currenciesStr)
    pretty(render(serialized))
    
  }
  private def sellBuyPack(name : String, cur : String, count : Int, price : Double) =
    ("name"->name)~("currency"->cur)~("count"->count)~("price"->price)
  private def operationPack(oper : Operation):JValue =
    oper match {
      case Selling(name, cur, count, price) =>
        ("type" -> "Selling") ~ ( "operation" -> sellBuyPack(name, cur, count, price))
      case Purchase(name, cur, count, price) =>
        ("type" -> "Selling") ~ ( "operation" -> sellBuyPack(name, cur, count, price))
      case _ =>
        "type" -> "error"
    }
  def operationToJson(oper : Operation) :String = pretty(render(operationPack(oper)))
  def operationsToJson(opers : List[Operation]) : String = pretty(render(opers.map(operationPack)))
}
