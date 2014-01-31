/*
	Processes definition class
*/

//sealed : all inherited case classes have to be implemented in this file
abstract sealed class Proc() {
	def toString()
}

case class ProcIn(c : String, x : Term) extends Proc {
	val channel = c 
	val store = x

	def toString() = {
		println("in(" + c + "," + x.toString() + ")")
	}
}