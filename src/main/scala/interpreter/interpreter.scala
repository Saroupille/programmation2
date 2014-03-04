/**
* interpreter.scala - threading of processes
* @author Lanvin Victor Thiré François
* Copyright (c) 2014 GPLv3. See LICENCE file
*/

import scala.collection.mutable.Map

class Interpreter(synch : Boolean) {
	//We define one strategy for all the channels
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

		//Parsing
		val ast = parser.parseFile(file); 
		//Threading
		val procs = threading(ast);
		//Interpretation
		run(procs);
	}
}
