package Operation
sealed abstract class Operation

case class EmptyOperation() extends Operation

case class Purchase(name : String, currency : String, count : Int, price : Double) extends Operation
case class Selling(name : String, currency : String, count : Int, price : Double) extends Operation
case class CurrencyRemoval(currency : String, price : Double = 1.0) extends Operation
case class CurrencyAddition(currency : String) extends Operation
case class ClientAddition(name : String, balance : Double) extends Operation
case class ClientRemoval(name : String) extends Operation

object OperationParser{
  def parseLine(line : String, lineParser: String => Operation) : Operation = lineParser(line)
  def parseLines(lines : List[String], lineParser: String => Operation) : List[Operation] = lines.map(lineParser)
}