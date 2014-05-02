import scala.util.Random

trait Group[E] {
  val unit : E
  val generator : E
  val order : BigInt

  def combines(e1: E, e2: E): E

  def toString(e : E): String 
  def fromString(s : String) : E
  def toBigInt(e : E) : BigInt
  def fromBigInt(n : BigInt) : E

  def exp (e: E, n: BigInt): E = {
    if(n==0)
      return unit
    else if(n==1)
      return e
    else if(n%2==0)
      return exp(combines(e,e), n/2)
    else
      return combines(e,exp(combines(e,e), (n-1)/2))
  }
}
