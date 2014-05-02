import scala.util.Random

trait Field[E] {
  val groupadd : Group[E]
  val groupx : Group[E]

  def add(e1:E, e2: E): E = groupadd.combines(e1,e2)
  def times(e1: E, e2: E): E = groupx.combines(e1,e2)
  def invert(e:E) : E
  def minus(e:E): E

  def multiply (e: E, n: BigInt): E = groupadd.exp(e,n)
  def exp (e: E, n: BigInt): E = groupx.exp(e,n)
}

class ZField (p:BigInt) extends Field[BigInt] {
  type E = BigInt

  val groupadd = new Zadd(p)
  val groupx = new Zmult(p)

  def invert(e:E) : E = exp(e, (groupx.order) - 1)
  def minus(e:E) : E = multiply(e, (groupadd.order) - 1)
}