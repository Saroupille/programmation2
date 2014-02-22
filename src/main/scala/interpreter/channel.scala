import scala.collection.mutable.SynchronizedQueue

class Channel(name : String, strategy:CalculationStrategy) {
  val strategy_m=strategy
  val name_m=name

  val queue=new SynchronizedQueue[String]()

  override def toString():String = {
    return "Channel("+name_m+","+strategy_m.toString+")"
  }

}
