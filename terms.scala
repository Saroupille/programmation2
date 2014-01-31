/*
	Terms definition class
*/
abstract class Term() {
	def toString : String
}

case class TermPair(t1 : Term, t2 : Term) extends Term {
	val leftTerm = t1
	val rightTerm = t2

	override def toString : String = {
		return "Pair(" + t1.toString() + "," + t2.toString() + ")"
	}
}

case class TermVariable(v : String) extends Term {
	val id = v

	override def toString : String = {
		return id
	}
}