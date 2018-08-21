package oper
sealed trait Operation

sealed trait QueuedOperation
case class Purchase(name : String, currency : String, count : Int, price : Double) extends QueuedOperation
case class Selling(name : String, currency : String, count : Int, price : Double) extends QueuedOperation

case class CurrencyAddition(currency : String, startCount : Int = 0)
case class CurrencyRemoval(currency : String, price : Double = 0)

case class ClientRemoval(name : String)

object OperationParser{
  def parseLine(line : String, lineParser: String => QueuedOperation) : QueuedOperation = lineParser(line)
  def parseLines(lines : List[String], lineParser: String => QueuedOperation) : List[QueuedOperation] = lines.map(lineParser)
}