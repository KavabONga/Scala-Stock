package resp

import req.RequestParser
import OrderExecutor.OrderExecutor
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives.complete
import serializer.ObjectsToJson

import scala.util.{Failure, Success, Try}

object PostToResponse {
  def success() =
    complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.success()))
  def error(exc : Throwable) =
    complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.error(exc)))
  private def toComplete[T](t : Try[T]) = {
    t match {
      case Success(_) =>
        success()
      case Failure(exc) =>
        error(exc)
    }
  }

  def buy(m : Map[String, Any])(implicit ex : OrderExecutor) = {
    val buyTry = RequestParser.tryToPurchase(m).flatMap(s =>
      Try(
        ex.request(s)
      )
    )
    toComplete(buyTry)
  }
  def sell(m : Map[String, Any])(implicit ex : OrderExecutor) = {
    val sellTry = RequestParser.tryToSelling(m).flatMap(s =>
      Try(
        ex.request(s)
      )
    )
    toComplete(sellTry)
  }
  def addClient(m : Map[String, Any])(implicit ex : OrderExecutor) = {
    val addClientTry = RequestParser.tryToClient(m).flatMap(client =>
      Try(
        ex.addClient(client)
      )
    )
    toComplete(addClientTry)
  }
  def removeClient(m : Map[String, Any])(implicit ex : OrderExecutor) = {
    val removeClientTry = RequestParser.tryToClientRemoval(m).flatMap(removal =>
      Try(
        ex.removeClient(removal.name)
      )
    )
    toComplete(removeClientTry)
  }
  def addCurrency(m : Map[String, Any])(implicit ex : OrderExecutor) = {
    val addCurrencyTry = RequestParser.tryToCurrencyAddition(m).flatMap(curAdd =>
      Try(
        ex.addCurrency(curAdd.currency, curAdd.startCount)
      )
    )
    toComplete(addCurrencyTry)
  }
  def removeCurrency(m : Map[String, Any])(implicit ex : OrderExecutor) = {
    val removeCurrencyTry = RequestParser.tryToCurrencyRemoval(m).flatMap( remCur =>
      Try(
        ex.removeCurrency(remCur.currency, remCur.price)
      )
    )
    toComplete(removeCurrencyTry)
  }
}
