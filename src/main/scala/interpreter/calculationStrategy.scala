import scala.collection.mutable.SynchronizedQueue
import scala.concurrent._
import ExecutionContext.Implicits.global


trait CalculationStrategy {
  def toString(): String;
  def write(sq:SynchronizedQueue[String], out:String, lockRead : Object, lockWrite : Object) : Unit
  def read(sq:SynchronizedQueue[String], lockRead : Object, lockWrite : Object):String
}


class SynchroneousStrategy extends CalculationStrategy {
  
  override def toString(): String = {
    return "Synchroneous"
  }

  def write(sq:SynchronizedQueue[String], out:String, lockRead : Object, lockWrite : Object) = {
    lockWrite.synchronized {
      while(sq.length >= 1)
        lockWrite.wait()
    }
    lockRead.synchronized {
      sq.enqueue(out);
      lockRead.notify();
      lockRead.wait()
    }
  }

  def read(sq:SynchronizedQueue[String], lockRead : Object, lockWrite : Object):String = {
    var res = "";
    lockRead.synchronized {
      while(sq.length == 0)
        lockRead.wait();
      res = sq.dequeue();
      lockRead.notifyAll()
    }
    lockWrite.synchronized {
      lockWrite.notify();
      return res
    }
  }
}

class AsynchroneousStrategy extends CalculationStrategy {

  def write(sq:SynchronizedQueue[String], out:String, lockRead : Object, lockWrite : Object) = {
    lockRead.synchronized {
      sq.enqueue(out);
      lockRead.notify();
    }
    lockWrite.synchronized { //Ici on se synchronise sur lockWrite juste pour attendre 10ms et éviter des "race conditions"
      lockWrite.wait(10);
    }
  }

  def read(sq:SynchronizedQueue[String], lockRead : Object, lockWrite : Object):String = {
    lockRead.synchronized {
      while(sq.length == 0)
        lockRead.wait();
      return sq.dequeue()
    }
  }

  override def toString(): String = {
    return "Asynchroneous"
  }
}
