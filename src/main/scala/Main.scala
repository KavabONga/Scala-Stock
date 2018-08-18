import ClientHandler.ClientHandler
import Operation._
import serializer.ObjectsToJson
import org.json4s.jackson.JsonMethods.{compact, render}
import org.json4s.JsonDSL._
import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.{Http, server}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import org.json4s.JsonDSL
import org.json4s.jackson.{Json, JsonMethods}

import scala.io.StdIn
import scala.util._
import OrderExecutor.OrderExecutor

object ServerMain extends App{
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.dispatcher
  var ex = new OrderExecutor(List())
  val route: Route =
  pathPrefix("addClient") {
    parameterMap { m =>
      val addTry = RequestParser.tryToClient(m).flatMap(client => {
        Try({
          ex = ex.addClient(client)
        })
      })
      addTry match {
        case Success(_) => complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.success))
        case Failure(exc) => complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.error(exc)))
      }
    }
  } ~
    pathPrefix("getClient") {
      parameter('name) { name =>
        val tryClient = Try(ex.getClient(name))
        tryClient match {
          case Success(client) => complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.clientToJson(client)))
          case Failure(exc) => complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.error(exc)))
        }
      }
    } ~
      pathPrefix("sell") {
        parameterMap { m =>
          val sellTry = RequestParser.tryToSelling(m).flatMap(s => {
            Try({
              ex = ex.request(s)
            })
          })
          sellTry match {
            case Success(_) => complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.success))
            case Failure(exc) => complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.error(exc)))
          }
        }
      } ~
        pathPrefix("buy") {
          parameterMap { m =>
            val buyTry = RequestParser.tryToPurchase(m).flatMap(s => {
              Try({
                ex = ex.request(s)
              })
            })
            buyTry match {
              case Success(_) => complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.success))
              case Failure(exc) => complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.error(exc)))
            }
          }
        } ~
          pathPrefix("getRequestQueue") {
            complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.operationsToJson(ex.getRequestQueue())))
          } ~
            pathPrefix("getClients") {
            complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.clientsToJson(ex.getClients)))
          }
  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
  StdIn.readLine() // let it run until user presses return
  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ => system.terminate()) // and shutdown when done
}