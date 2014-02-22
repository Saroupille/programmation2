import scala.collection.mutable.SynchronizedQueue
import scala.concurrent._
import ExecutionContext.Implicits.global

trait CalculationStrategy {
  def toString(): String;
}


class SynchroneousStrategy extends CalculationStrategy {
  
  override def toString(): String = {
    return "Synchroneous"
  }
}

class AsynchroneousStrategy extends CalculationStrategy {
  
  def write(sq:SynchronizedQueue[String], out:String) = {
    sq.enqueue(out)
  }

  def read(sq:SynchronizedQueue[String]):String = {
    val f: Future[String] = future(
      sq.dequeue()
    )
    var truc=""
    f onSuccess {
      case msg => truc= msg
    }
    return truc
  }

  override def toString(): String = {
    return "Asynchroneous"
  }
}
