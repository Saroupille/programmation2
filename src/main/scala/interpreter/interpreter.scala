import scala.collection.mutable.Map

class Interpreter(synch : Boolean) {
	val strategy : CalculationStrategy = 
		if (synch) 
			new SynchroneousStrategy() 
		else 
			new AsynchroneousStrategy()

	val parser : Parser = new Parser(strategy)

	def interprete(file : String) : Unit = {
		def threading(l : List[Proc]) : List[Thread] = {
			l match {
				case Nil => Nil
				case t::q => (new Thread(new Runnable{def run () = {t.interprete(Map())}})) :: threading(q)
			}
		}

		def run(l : List[Thread]) : Unit = {
			l match {
				case Nil => ()
				case t::q => t.start; run(q)
			}
		}

		val ast = parser.parseFile(file);
		val procs = threading(ast);
		run(procs);
	}
}