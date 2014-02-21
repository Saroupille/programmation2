object Test {

  def interpretationTest() : Unit = { 
    println("###################")
    println("Unit tests on terms")
    println("###################")
    val x= TermVariable("x")
    println("Result expected : x")
    println("Result          : "+x.interprete)
    val y= TermVariable("y")
    val pair1= TermPair(x,y)
    println("Result expected : pair(x,y)")
    println("Result          : "+pair1.interprete)
    val proj1=TermProj1(pair1)
    println("Result expected : x")
    println("Result          : "+proj1.interprete)
    val proj2=TermProj2(pair1)
    println("Result expected : y")
    println("Result          : "+proj2.interprete)
    val list1=TermList(List(x,y,pair1,proj1,proj2))
    println("Result expected : x::y::pair(x,y)::x::y::[]")
    println("Result          : "+list1.interprete)
  }

  def parseTest() : Unit = {
    def printTest (l : List[Proc]) : Unit = {
        l match {
            case Nil => println ("");
            case t::q => println (t.toString()); printTest(q);
        }
    }

    val parser = new Parser();
    val l = parser.parseFile("testNSL.txt");
    printTest(l);
  }
}
