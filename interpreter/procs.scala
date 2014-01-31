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
	val channel = c 
	val store = x

	override def toString : String = {
		return "in(" + channel + "," + store.toString() + ")"
	}
}
