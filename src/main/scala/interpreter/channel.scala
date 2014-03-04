/**
* channel.scala - Definitions of the generic channel class
* @author Lanvin Victor Thiré François
* Copyright (c) 2014 GPLv3. See LICENCE file
*/

import scala.collection.mutable.SynchronizedQueue

//One channel for both strategies
class Channel(name : String, strategy:CalculationStrategy) {
  val strategy_m = strategy
  val name_m = name
  val queue_m = new SynchronizedQueue[String]()

  // Locks must be created by the channel, because the strategy is shared by all channels
  val lockRead_m : Object = new Object();  
  val lockWrite_m : Object = new Object();

  def read() : String = {
    return strategy_m.read(queue_m, lockRead_m, lockWrite_m);
  }

  def write(str : String) : Unit = {
  	if(name_m == "stdout")
  		println(str);
    else {
      strategy_m.write(queue_m, str, lockRead_m, lockWrite_m);
    }
  }

  override def toString():String = {
    return "Channel("+name_m+","+strategy_m.toString+")"
  }

}
