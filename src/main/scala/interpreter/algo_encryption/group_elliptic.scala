/*class Elliptic (g:Field[El], eq:Array[El]) extends Group[BigInt] {

  
    Equation : y^2 + eq[0]*xy + eq[2]*y = x^3 + eq[1]*x^2 + eq[3]*x + eq[4]
  

  abstract class Elt 
  case class Infinite extends Elt
  case class Point(x:El,y:El) extends Elt

  type E = Elt
  val unit = new Infinite
  val generator = Point(eq(4), g.groupadd.unit)
  val order = 0

  def toString(e : E) : String = {
    e.toString
  }

  def fromString(s : String) : E = {
    BigInt(s)
  }

  def combines(a:E, b:E) : E = {
    return ((a+b)%order)
  }

  def toBigInt(e : E) : BigInt = {
    return e
  }

  def fromBigInt(n : BigInt) : E = {
    return n % order
  }
}
 */
