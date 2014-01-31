/*
	Values definition class
*/

abstract case class Value() extends Term {
	
}

case class ValueInteger(v : Int) extends Value {
	val value = v

	override def toString : String = {
		return value.toString
	}
}
