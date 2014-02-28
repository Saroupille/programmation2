import scala.collection.mutable.SynchronizedQueue

class Channel(name : String, strategy:CalculationStrategy) {
  val strategy_m=strategy
  val name_m=name

  val queue=new SynchronizedQueue[String]()

  def read() : String = {
  	val temp = strategy_m.read(queue);
  	return temp;
  }

  def write(str : String) : Unit = {
  	if(name_m == "stdout")
  		println(str);
  	strategy_m.write(queue, str);
  }

  override def toString():String = {
    return "Channel("+name_m+","+strategy_m.toString+")"
  }

}
