/**
* procs.scala - Definitions of different case classes to represent processes
* @author Lanvin Victor Thiré François
* Copyright (c) 2014 GPLv3. See LICENCE file
*/

/*
  Note : Processes always take the next process as an argument (maybe Zero)
*/

import scala.collection.mutable.Map
import util.Random

//sealed : all inherited case classes have to be implemented in this file
abstract sealed class Proc() {
  def interprete(env : Map[String,String]) : Unit
}

//Not-so-useless class
case class ProcZero() extends Proc {
  def interprete(env : Map[String,String]) : Unit = {
  }
}


//Process In(c,x)
case class ProcIn(channel_m : Channel, store_m : String, procNext_m : Proc) extends Proc {

  def interprete(env : Map[String,String]) : Unit = {
    env.put(store_m, channel_m.read()); //Adding the message to the environment
    procNext_m.interprete(env);
  }
}


//Process In^k(c, x -> u as y)
case class ProcInK(
  iterate_m : Int,          //k
  channel_m : Channel,      //c
  functionArg_m : String,   //x
  functionRes_m : Term,     //u
  variable_m : String,      //y
  procNext_m : Proc) extends Proc {
  
  def interprete(env : Map[String,String]) : Unit = {
    //Iterate the process as specified by the semantics
    def aux(iterations : Int) : String = { 
      if (iterations == 0) {
        return "[]"
      }
      else {
        //Make a temporary environment to interprete u
        var tmpEnv : Map[String,String] = Map(); 
        tmpEnv.put(functionArg_m, channel_m.read());
        val interpRes = functionRes_m.interprete(tmpEnv);
        //Create the list y
        return interpRes + "::" + aux(iterations-1);
      }
    }
    env.put(variable_m, aux(iterate_m));
    procNext_m.interprete(env);
  }
}


//Process Out(c,m)
case class ProcOut(channel_m : Channel, message_m : Term, procNext_m : Proc) extends Proc {

  def interprete(env : Map[String,String]) : Unit = {
    channel_m.write(message_m.interprete(env));
    procNext_m.interprete(env);
  }
}


//Process if V then P else P
case class ProcIf(value_m : Value, then_m : Proc, else_m : Proc) extends Proc {
  
  def interprete(env : Map[String,String]) : Unit = {
    if (value_m.interprete(env) != "0")
      then_m.interprete(env)
    else
      else_m.interprete(env)
  }
}


//Process new v
case class ProcNew(value_m : ValueConst, procNext_m : Proc) extends Proc {
  
  def interprete(env : Map[String,String]) : Unit = {
    val v = value_m.interprete(env);
    procNext_m.interprete(env);
  }
}
