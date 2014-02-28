import scala.collection.mutable.SynchronizedQueue
import scala.concurrent._
import ExecutionContext.Implicits.global

trait CalculationStrategy {
  def toString(): String;
  def write(sq:SynchronizedQueue[String], out:String) : Unit
  def read(sq:SynchronizedQueue[String]):String
}


class SynchroneousStrategy extends CalculationStrategy {
  
  override def toString(): String = {
    return "Synchroneous"
  }

  //Pour l'instant j'ai juste copié collé la stratégie asynchrone pour éviter les problèmes d'abstract class..
  def write(sq:SynchronizedQueue[String], out:String) = {
    sq.enqueue(out)
  }

  def read(sq:SynchronizedQueue[String]):String = {
    try {
      return sq.dequeue()
    }
    catch {
      case _ : Throwable => read(sq)
    }
  }
}

class AsynchroneousStrategy extends CalculationStrategy {
  
  def write(sq:SynchronizedQueue[String], out:String) = {
    sq.enqueue(out)
  }

  def read(sq:SynchronizedQueue[String]):String = {
    /*val f: Future[String] = future {
      sq.dequeue()
    }

    var temp = "err"
    f onSuccess {
      case msg => temp = msg
    }

    return temp*/
    try {
      return sq.dequeue()
    }
    catch {
      case _ : Throwable => read(sq)
    }
  }

  override def toString(): String = {
    return "Asynchroneous"
  }
}
