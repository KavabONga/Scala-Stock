abstract class a{
  val x : Int
  val y : Int
}
case class b(x : Int, y : Int) extends a
case class c(x : Int, y : Int) extends a
b(1, 2) == b(1, 2)