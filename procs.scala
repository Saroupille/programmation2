/*
	Processes definition class
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