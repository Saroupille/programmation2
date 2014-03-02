/**
* values.scala - Definitions of different case classes to represent a value
* @author Lanvin Victor Thiré François
* Copyright (c) 2014 GPLv3. See LICENCE file
*/

import scala.collection.mutable.Map
import util.Random

//sealed : all inherited case classes have to be implemented in this file
abstract sealed class Proc() {
  def interprete(env : Map[String,String]) : Unit
}
//can be an object ?
case class ProcZero() extends Proc {
  def interprete(env : Map[String,String]) : Unit = {
  }
}

case class ProcIn(channel_m : Channel, store_m : String, procNext_m : Proc) extends Proc {
  
  def interprete(env : Map[String,String]) : Unit = {
    env.put(store_m, channel_m.read());
    procNext_m.interprete(env);
  }
}

case class ProcInK(
  iterate_m : Int,
  channel_m : Channel, 
  functionArg_m : String, 
  functionRes_m : Term, 
  variable_m : String, 
  procNext_m : Proc) extends Proc {
  
  def interprete(env : Map[String,String]) : Unit = {
    def aux(iterations : Int) : String = {
      if (iterations == 0) {
        return "[]"
      }
      else {
        var tmpEnv : Map[String,String] = Map();
        tmpEnv.put(functionArg_m, channel_m.read());
        val interpRes = functionRes_m.interprete(tmpEnv);
        return interpRes + "::" + aux(iterations-1);
      }
    }
    env.put(variable_m, aux(iterate_m));
    procNext_m.interprete(env);
  }
}

case class ProcOut(channel_m : Channel, message_m : Term, procNext_m : Proc) extends Proc {
  
  def interprete(env : Map[String,String]) : Unit = {
    channel_m.write(message_m.interprete(env));
    procNext_m.interprete(env);
  }
}

case class ProcIf(value_m : Value, then_m : Proc, else_m : Proc) extends Proc {
  
  def interprete(env : Map[String,String]) : Unit = {
    if (value_m.interprete(env) != "0")
      then_m.interprete(env)
    else
      else_m.interprete(env)
  }
}

case class ProcNew(value_m : ValueConst, procNext_m : Proc) extends Proc {
  
  def interprete(env : Map[String,String]) : Unit = {
    val v = value_m.interprete(env);
    procNext_m.interprete(env);
  }
}
