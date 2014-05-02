class Zadd (n:BigInt) extends Group[BigInt] {
  type E = BigInt
  val unit=BigInt(0)
  val generator=BigInt(1)
  val order=n

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
