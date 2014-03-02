import scala.collection.mutable.SynchronizedQueue

class Channel(name : String, strategy:CalculationStrategy) {
  val strategy_m = strategy
  val name_m = name
  val queue = new SynchronizedQueue[String]()

  val lockRead : Object = new Object();
  val lockWrite : Object = new Object();

  def read() : String = {
    return strategy_m.read(queue, lockRead, lockWrite);
  }

  def write(str : String) : Unit = {
  	if(name_m == "stdout")
  		println(str);
    else {
      strategy_m.write(queue, str, lockRead, lockWrite);
    }
  }

  override def toString():String = {
    return "Channel("+name_m+","+strategy_m.toString+")"
  }

}
