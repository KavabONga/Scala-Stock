package StockDB

import ClientHandler.ClientHandler
import com.typesafe.scalalogging.LazyLogging
import scala.concurrent.Future
import scala.concurrent.Future._
import scala.concurrent.ExecutionContext.Implicits.global
import slick.jdbc.SQLiteProfile.api._
import collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import slick.lifted.Rep
import slick.lifted.FunctionSymbolExtensionMethods._
object StockTables {
  class Clients(tag: Tag) extends Table[(String, Double)](tag, "CLIENTS") {
    def name = column[String]("NAME", O.PrimaryKey)
    def balance = column[Double]("PRICE")
    def * = (name, balance)
    // A reified foreign key relation that can be navigated to create a join
  }
  val clients = TableQuery[Clients]
  class ClientsToCurrencies(tag : Tag) extends Table[(String, String, Int)](tag, "CLIENTS_TO_CURRENCIES") {
    def clientName = column[String]("CLIENT_NAME")
    def currencyName = column[String]("CURRENCY_NAME")
    def currencyCount = column[Int]("CURRENCY_COUNT")
    def * = (clientName, currencyName, currencyCount)

    def client = foreignKey("CLIENT_FK", clientName, clients)(_.name)
  }
  val clientsToCurrencies = TableQuery[ClientsToCurrencies]
}

object StockDBRunner {
  import StockTables._
  def clearClientsReq() = {
    clients.schema.drop >> clients.schema.create
  }
  def clearClientsToCurrenciesReq() = {
    clientsToCurrencies.schema.drop >> clientsToCurrencies.schema.create
  }
  def clearTablesReq() = {
    clearClientsReq() >> clearClientsToCurrenciesReq()
  }
  def clearClients(implicit db : Database) = {
    db.run(clearClientsReq)
  }
  def clearClientsToCurrencies(implicit db : Database) = {
    db.run(clearClientsToCurrenciesReq())
  }
  def clearTables(implicit db : Database) = {
    db.run(clearTablesReq)
  }
  def addClientReq(client : ClientHandler) = {
    (clients += (client.name, client.balance)) >> (clientsToCurrencies ++= client.currencies.map(p => (client.name, p._1, p._2)))
  }
  def getClientRowReq(name : String) = {
     clients.filter(_.name === name).result.head
  }
  def getClientCurrenciesReq(name : String) = {
    clientsToCurrencies.filter(_.clientName === name).result
  }
  def addClient(client : ClientHandler)(implicit db : Database) = {
    db.run(addClientReq(client))
  }
  def getClient(name : String)(implicit db : Database) = {
    val client = db.run(getClientRowReq(name))
    val clientCurrenciesList = db.run(getClientCurrenciesReq(name))
    val clientCurrenciesMap = clientCurrenciesList.map(curs => {
      curs.toList.map(oneCur => oneCur._2 -> oneCur._3).toMap
    })
    Future.sequence(List(client, clientCurrenciesMap)).map(p => {
      val clientPair = p.head match {
        case t : (String, Double) => t
        case _ => throw new Exception
      }
      val currencies = p(1) match {
        case m : Map[String, Int] => m
        case _ => throw new Exception
      }
      new ClientHandler(clientPair._1, clientPair._2, mutable.Map.empty ++ currencies)
    })
  }
  def getClients(implicit db : Database) = {
    val namesReq = for (
      client <- clients
    ) yield client.name
    db.run(namesReq.result).map(l => {
      Future.sequence(l.map(s => getClient(s)).toList)
    }).flatten
  }
}

object DBMain extends App with LazyLogging{
  import StockDBRunner._
  implicit val db = Database.forURL(
    "jdbc:sqlite:C:\\Users\\mitya\\Documents\\Git_workspace\\Scala-Stock\\src\\main\\resources\\test.db",
    driver = "org.sqlite.JDBC"
  )
  val actions = DBIO.seq(
    StockTables.clientsToCurrencies.schema.create
  )
  println(Await.result(getClients, 2 seconds))
}
