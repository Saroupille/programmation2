import scala.collection.mutable.Map

object Test {

  def interpretationTest() : Unit = { 
    println("###################")
    println("Unit tests on terms")
    println("###################")
    val x= TermVariable("x")
    println("Result expected : x")
    println("Result          : "+x.interprete(Map()))
    val y= TermVariable("y")
    val pair1= TermPair(x,y)
    println("Result expected : pair(x,y)")
    println("Result          : "+pair1.interprete(Map()))
    val proj1=TermProj1(pair1)
    println("Result expected : x")
    println("Result          : "+proj1.interprete(Map()))
    val proj2=TermProj2(pair1)
    println("Result expected : y")
    println("Result          : "+proj2.interprete(Map()))
    val list1=TermList(List(x,y,pair1,proj1,proj2))
    println("Result expected : x::y::pair(x,y)::x::y::[]")
    println("Result          : "+list1.interprete(Map()))
  }

  def parseTest() : Unit = {
    def printTest (l : List[Proc]) : Unit = {
        l match {
            case Nil => println ("");
            case t::q => println(t.toString()); printTest(q)
        }
    }

    def interpreteTest (l: List[Proc]) : Unit = {
        l match {
            case Nil => println ("End of interpretation");
            case t::q => t.interprete(Map()); printTest(q)
        }
    }

    val parser = new Parser();
    val l = parser.parseFile("testNSL.txt");
    printTest(l);
    interpreteTest(l);
  }
}
