import ClientHandler.ClientHandler
import Operation._
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
import serializer.ObjectsToJson



object Main extends App{
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
            client
          })
        })
        addTry match {
          case Success(client) => complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"Client with name ${client.name} and balance ${client.balance} added"))
          case Failure(exc) => complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, exc.getMessage))
        }
      }
    } ~
      pathPrefix("getClient") {
        parameter('name) { name =>
          val tryClient = Try(ex.getClient(name))
          tryClient match {
            case Success(client) => complete(HttpEntity(ContentTypes.`application/json`, ObjectsToJson.clientToJson(client)))
            case Failure(exc) => complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, exc.getMessage))
          }
        }
      } ~
      pathPrefix("sell") {
        parameterMap { m =>
          val sellTry = RequestParser.tryToSelling(m).flatMap(s => {
            Try({
              ex = ex.request(s)
              s
            })
          })
          sellTry match {
            case Success(selling) => complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"${selling.name} successfully published his selling request"))
            case Failure(exc) => complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, exc.getMessage))
          }
        }
      } ~
      pathPrefix("buy") {
        parameterMap { m =>
          val buyTry = RequestParser.tryToPurchase(m).flatMap(s => {
            Try({
              ex = ex.request(s)
              s
            })
          })
          buyTry match {
            case Success(purchase) => complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"${purchase.name} successfully published his purchase request"))
            case Failure(exc) => complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, exc.getMessage))
          }
        }
      } ~
      pathPrefix("getRequestQueue") {
        complete(ex.getRequestQueue().map(ObjectsToJson.operationToJson).mkString(",\n"))
      } ~
      pathPrefix("getClients") {
        complete(ex.getClients.mkString(",\n"))
      }
  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
  StdIn.readLine() // let it run until user presses return
  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ => system.terminate()) // and shutdown when done
}