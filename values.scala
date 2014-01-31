/*
	Values definition class
*/

abstract case class Value() extends Term {

}

// Extractor class for integer values
class ValueInteger(v : Int) extends Value {
	val value = v

	override def toString : String = {
		return value.toString
	}
}
object ValueInteger {
	def unapply(vi : ValueInteger): Option[Int] = Some(vi.value)
}

// Extractor class for constant values
class ValueConst(s : String) extends Value {
	val identifier = s

	override def toString : String = {
		return identifier
	}
}
object ValueConst {
	def unapply(vc : ValueConst): Option[String] = Some(vc.identifier)
}


//Some tests
object Test extends App {
	val p = new ValueInteger(10)
	val p2 = new ValueConst("NomVal")
	val p1 = new ValueInteger(15)

	p2 match {
		case ValueInteger(n) => println ("Valeur de p2 : " + n)
		case ValueConst(str) => println ("Nom de p2 : " + str)
	}

	p match {
		case ValueInteger(n) => println ("Valeur de p : " + n)
		case ValueConst(str) => println ("Nom de p : " + str)
	}

	p1 match {
		case ValueInteger(n) => println ("Valeur de p1 : " + n)
		case ValueConst(str) => println ("Nom de p1 : " + str)
	}
}