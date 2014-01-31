/**
  * values.scala - Definitions of different case classes to represent a value
  * @author Lanvin Victor Thiré François
  * Copyright (c) 2014 GPLv3. See LICENCE file
  */


abstract class Term() {
	def toString : String
}

case class TermValue(v: Value) extends Term {
  val value=v

  override def toString : String = {
    return v.toString
  }
}

case class TermVariable(v : String) extends Term {
	val variable = v

	override def toString : String = {
		return variable
	}
}

//Probably not Any
case class TermList(l: List[Any]) extends Term {
  val list=l

  override def toString : String = {
    return list.toString
  }
}

case class TermPair(t1 : Term, t2 : Term) extends Term {
	val leftTerm_m = t1
	val rightTerm_m = t2

	override def toString : String = {
		return "Pair(" + leftTerm_m.toString() + "," + rightTerm_m.toString() + ")"
	}
}
