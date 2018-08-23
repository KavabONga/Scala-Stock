import concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import scala.util._
Future(1) onComplete {
  _ => println(1)
}