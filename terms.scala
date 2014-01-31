/*
	Terms definition class
*/

//sealed : all inherited case classes have to be implemented in this file
abstract sealed class Term() {
	def toString()
}

case class Pair(t1 : Term, t2 : Term) extends Term {
	val leftTerm = t1
	val rightTerm = t2

	def toString() = {
		println("Pair(" + t1.toString() + "," + t2.toString() + ")")
	}
}

case class Variable(v : String) extends Term {
	val id = v

	def toString() = {
		println(id);
	}
}

case class Value(v : int) extends Term {
	val value = v

	def toString() = {
		println(v);
	}
}