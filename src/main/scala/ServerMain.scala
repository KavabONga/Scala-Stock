import resp.PostToResponse
import serializer.ObjectsToJson
import org.json4s.jackson.JsonMethods._
import akka.actor.ActorSystem
import akka.http.scaladsl.server.{Route, StandardRoute}
import akka.http.scaladsl.{Http, server}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives.{post => postWrapper, get => getWrapper}
import akka.http.scaladsl.server.Directives._
import akka.stream.{ActorMaterializer, Materializer}
import org.json4s.DefaultFormats

import scala.util._
import OrderExecutor.OrderExecutor
import akka.http.scaladsl.unmarshalling.Unmarshal

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.duration.Duration._
import scala.concurrent.ExecutionContext.Implicits.global


object FinalResponse {
  def mapFromReq(req : HttpMessage)(implicit mat:Materializer, formats : DefaultFormats) = {
    val RespString = Unmarshal(req.entity).to[String]
    val resp = Await.result(RespString, Duration(100, MILLISECONDS))
    Try(parse(resp).extract[Map[String, Any]])
  }
  def applyMethodWithResponse(req : HttpMessage, method : Map[String, Any] => StandardRoute)(implicit mat:Materializer, formats : DefaultFormats): StandardRoute = {
    mapFromReq(req) match {
      case Success(m) =>
        method(m)
      case Failure(exc) =>
        complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.error(exc)))
    }
  }
}

object StockRouter {
  import FinalResponse._

  def addClient(implicit mat:Materializer, formats : DefaultFormats, ex : OrderExecutor) =
    pathPrefix("addClient") {
      formFieldMap { m =>
        PostToResponse.addClient(m)
      } ~
        extractRequest { req =>
          applyMethodWithResponse(req, PostToResponse.addClient)
        }
    }
  def removeClient(implicit mat:Materializer, formats : DefaultFormats, ex : OrderExecutor) =
    pathPrefix("removeClient") {
      formFieldMap { m =>
        PostToResponse.removeClient(m)
      } ~
        extractRequest { req =>
          applyMethodWithResponse(req, PostToResponse.removeClient)
        }
    }
  def sell(implicit mat:Materializer, formats : DefaultFormats, ex : OrderExecutor) =
    pathPrefix("sell") {
      formFieldMap { m =>
        PostToResponse.sell(m)
      } ~
        extractRequest {req =>
          applyMethodWithResponse(req, PostToResponse.sell)
        }
    }
  def buy(implicit mat:Materializer, formats : DefaultFormats, ex : OrderExecutor) =
    pathPrefix("buy") {
      formFieldMap { m =>
        PostToResponse.buy(m)
      } ~
        extractRequest{ req =>
          applyMethodWithResponse(req, PostToResponse.buy)
        }
    }
  def addCurrency(implicit mat:Materializer, formats : DefaultFormats, ex : OrderExecutor) =
    pathPrefix("addCurrency") {
      formFieldMap { m =>
        PostToResponse.addCurrency(m)
      } ~
        extractRequest { req =>
          applyMethodWithResponse(req, PostToResponse.addCurrency)
        }
    }
  def removeCurrency(implicit mat:Materializer, formats : DefaultFormats, ex : OrderExecutor) =
    pathPrefix("removeCurrency") {
      formFieldMap { m =>
        PostToResponse.removeCurrency(m)
      } ~
        extractRequest { req =>
          applyMethodWithResponse(req, PostToResponse.removeCurrency)
        }
    }
  def post(implicit mat:Materializer, formats : DefaultFormats, ex : OrderExecutor) =
    postWrapper {
      addClient ~
        removeClient ~
        buy ~
        sell ~
        addCurrency ~
        removeCurrency
    }

  def getClient(implicit ex : OrderExecutor) =
    pathPrefix("getClient") {
      parameter('name) { name =>
        val tryClient = Try(ex.getClient(name))
        tryClient match {
          case Success(client) => complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.clientToJson(client)))
          case Failure(exc) => complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.error(exc)))
        }
      }
    }
  def getClients(implicit ex : OrderExecutor) =
    pathPrefix("getClients") {
      complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.clientsToJson(ex.getClients)))
    }
  def getRequestQueue(implicit ex : OrderExecutor) =
    pathPrefix("getRequestQueue") {
      complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.operationsToJson(ex.getRequestQueue)))
    }
  def get(implicit ex : OrderExecutor) =
    getWrapper {
      getClients ~
        getClient ~
        getRequestQueue
    }
}

object ServerMain extends App{
  implicit val formats: DefaultFormats.type = DefaultFormats
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  implicit val ex = new OrderExecutor()
  val route: Route =
    StockRouter.post ~ StockRouter.get
  /*ToDo:
    Add Client updating
    Add requesting the permission to lower the price of selling/ increase the price of purchase
  */
  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
}