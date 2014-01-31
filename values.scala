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


// Extractor class for count values
class ValueCount(l : List[Term]) extends Value {
	val list = l

	override def toString : String = {
		def listToString(l : List[Term]) : String =  {
			l match {
				case head :: tail =>  head.toString + "," + listToString(tail)
				case List() => "0"
			}
		}
		"count(" + listToString(list) + ")"
	}
}
object ValueCount {
	def unapply(vc : ValueCount): Option[List[Term]] = Some(vc.list)
}


//Some tests
object Test extends App {

	def printListTest(l : List[Term]) : String = {
		l match {
			case List() => ""
			case ValueConst(s) :: tail => {
				"Constant named " + s + "\n" +
				printListTest(tail)
			}
			case ValueInteger(i) :: tail => {
				"Integer value = " + i + "\n" +
				printListTest(tail)
			}
		}
	}

	val termList = List(
		new ValueConst("Const 1"), 
		new ValueConst("Const 2"), 
		new ValueInteger(15), 
		new ValueConst("Const 3"), 
		new ValueInteger(30), 
		new ValueInteger(40))

	val countTest = new ValueCount(termList)

	//test 1
	println(countTest.toString)

	//test 2
	println(printListTest(termList))
}