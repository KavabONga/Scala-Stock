package ClientHandler

class ClientHandler(val name : String, val balance : Double = 0, val currencies : Map[String, Int] = Map()) {
  def sell(currency : String, count : Int, price : Double): ClientHandler = {
    new ClientHandler(
      name,
      balance + count * price,
      currencies.map(p => {
        if (p._1 == currency) (p._1, p._2 - count)
        else (p._1, p._2)
      })
    )
  }
  def buy(currency : String, count : Int, price : Double) : ClientHandler = {
    new ClientHandler(
      name,
      balance - count * price,
      currencies.map(p => {
        if (p._1 == currency) (p._1, p._2 + count)
        else (p._1, p._2)
      })
    )
  }

  def addCurrency(currency : String, startCount : Int = 0) : ClientHandler = {
    new ClientHandler(
      name,
      balance,
      currencies + (currency -> startCount)
    )
  }
  def removeCurrency(currency : String, price : Double = 1.0) : ClientHandler = {
    new ClientHandler(
      name,
      balance + currencies.getOrElse(currency, 0) * price,
      currencies - currency
    )
  }

  def canSpend(money : Double): Boolean = balance >= money
  def canSell(currency: String, count : Int): Boolean = currencies.getOrElse(currency, 0) >= count

  def get(currency : String) : Int = currencies.getOrElse(currency, 0)

  override def toString: String = s"$name: $balance$$ | $currencies"
}

object ClientParser {
  def parseLine(line : String, lineParser : String => ClientHandler) : ClientHandler = lineParser(line)
  def parseLines(lines : List[String], lineParser : String => ClientHandler): List[ClientHandler] = lines.map(lineParser)
}