/**
* values.scala - Definitions of different case classes to represent a value
* @author Lanvin Victor Thiré François
* Copyright (c) 2014 GPLv3. See LICENCE file
*/

//sealed : all inherited case classes have to be implemented in this file
abstract sealed class Proc() {
	def toString : String
}

case class ProcIn(c : String, x : Term) extends Proc {
	val channel_m = c
	val store_m = x

	override def toString : String = {
		return "in(" + channel_m + "," + store_m.toString() + ")"
	}
}

case class ProcOut(c : String, m : Term) extends Proc {
	val channel_m = c
	val message_m = m

	override def toString : String = {
		return "out(" + channel_m + "," + message_m.toString() + ")"
	}
}

case class ProcIf(v : Value, p1 : Proc, p2 : Proc) extends Proc {
	val value_m = v
	val then_m = p1
	val else_m = p2

	override def toString : String = {
		return "if " + value_m.toString + " then " + then_m.toString + " else " + else_m.toString
	}
}

case class ProcNew(v : ValueConst) extends Proc {
	val value_m = v

	override def toString : String = {
		return "new " + value_m.toString
	}
}