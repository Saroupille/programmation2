/**
* calculationStrategy - Definition of the two strategies
* @author Lanvin Victor Thiré François
* Copyright (c) 2014 GPLv3. See LICENCE file
*/

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
    //We wait while a thread is already waiting to write in the channel
    lockWrite.synchronized { 
      while(sq.length >= 1)
        lockWrite.wait()
    }
    //We can now send the message, and wait for a recipient
    lockRead.synchronized {
      sq.enqueue(out);
      lockRead.notify(); //Notify a recipient waiting to read
      lockRead.wait() //And we wait for its response
    }
  }

  def read(sq:SynchronizedQueue[String], lockRead : Object, lockWrite : Object):String = {
    var res = "";
    lockRead.synchronized {
      while(sq.length == 0) //We wait while no writer is available
        lockRead.wait();
      res = sq.dequeue();   //A message has been sent, get it and notify the sender
      lockRead.notifyAll() 
    }
    lockWrite.synchronized {
      lockWrite.notify();   //Notify a waiting writer that he can now write
      return res
    }
  }
}

class AsynchroneousStrategy extends CalculationStrategy {
  //In fact, lockWrite is useless in this class

  def write(sq:SynchronizedQueue[String], out:String, lockRead : Object, lockWrite : Object) = {
    //We just put our message and notify a reader
    lockRead.synchronized {
      sq.enqueue(out);
      lockRead.notify();
    }
  }

  def read(sq:SynchronizedQueue[String], lockRead : Object, lockWrite : Object):String = {
    //We wait until a message is ready, and we get it
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
