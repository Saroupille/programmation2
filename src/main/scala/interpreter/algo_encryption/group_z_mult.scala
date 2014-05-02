class Zmult (p:BigInt) extends Group[BigInt] {
  type E = BigInt
  val unit=BigInt(1)
  val generator=BigInt(5)
  val order=p-1

  def toString(e : E) : String = {
  	e.toString
  }

  def fromString(s : String) : E = {
  	(BigInt(s) % order)
  }

  def combines(a:E, b:E) : E = {
    return ((a*b)%(order+1))
  }

  def toBigInt(e : E) : BigInt = {
  	return e
  }

  def fromBigInt(n : BigInt) : E = {
  	return (n % order)
  }
}
