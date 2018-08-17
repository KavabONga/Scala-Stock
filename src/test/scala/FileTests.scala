import ClientHandler._
import Operation._
import OrderExecutor.OrderExecutor

object Tester extends App{
  val path = "C:/Users/mitya/Documents/Git_workspace/Scala-Stock/src/test/cases/"
  var clientLines = scala.io.Source.fromFile(path + "clients.txt").getLines.toList
  val clients = ClientParser.parseLines(clientLines, line => {
    val ar = line.split(" ")
    new ClientHandler(ar(0), ar(1).toInt, Map("A" -> ar(2).toInt, "B" -> ar(3).toInt, "C" -> ar(4).toInt, "D" -> ar(5).toInt))
  })
  val operationLines = scala.io.Source.fromFile(path + "orders.txt").getLines.toList
  val operations = OperationParser.parseLines(operationLines, line => {
    val ar = line.split(" ")
    ar.head match {
      case "sell" => Selling(ar(1), ar(2), ar(3).toInt, ar(4).toDouble)
      case "buy" => Purchase(ar(1), ar(2), ar(3).toInt, ar(4).toDouble)
      case "addCurrency" => CurrencyAddition(ar(1))
      case "removeCurrency" => CurrencyRemoval(ar(1))
      case "addClient" => ClientAddition(ar(1), ar(2).toDouble)
      case "removeClient" => ClientRemoval(ar(1))
      case _ => EmptyOperation()
    }
  })
  var exec = new OrderExecutor(clients)
  operations.foreach(oper => exec = exec.request(oper))
  println(exec.getClients.sortBy(_.name).mkString("\n"))
  println()
  println(exec.getRequestQueue().mkString(" "))
}