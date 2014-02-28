/**
* values.scala - Definitions of different case classes to represent a value
* @author Lanvin Victor Thiré François
* Copyright (c) 2014 GPLv3. See LICENCE file
*/

import scala.collection.mutable.Map
import util.Random

//sealed : all inherited case classes have to be implemented in this file
abstract sealed class Proc() {
  def toString : String
  def interprete(env : Map[String,String]) : Unit
}

case class ProcZero() extends Proc {
  override def toString : String = {
    return "0"
  }

  def interprete(env : Map[String,String]) : Unit = {
  }
}

case class ProcIn(c : Channel, x : Term, p : Proc) extends Proc {
  val channel_m = c
  val store_m = x
  val procNext_m = p

  override def toString : String = {
    return "in(" + channel_m.toString() + "," + store_m.toString() + ")." + procNext_m.toString()
  }

  def interprete(env : Map[String,String]) : Unit = {
    
  }
}

case class ProcInK(k : Int, c : Channel, x : String, u : Term, y : TermVariable, p : Proc) extends Proc {
  val channel_m = c
  val functionArg_m = x
  val functionRes_m = u
  val variable_m = y
  val procNext_m = p

  override def toString : String = {
    return "in^" + k + "(" + channel_m.toString() + "," + functionArg_m + " --> " + functionRes_m.toString() + " as " + variable_m.toString() + ")." + procNext_m.toString()
  }

  def interprete(env : Map[String,String]) : Unit = {
    
  }
}

case class ProcOut(c : Channel, m : Term, p : Proc) extends Proc {
  val channel_m = c
  val message_m = m
  val procNext_m = p

  override def toString : String = {
    return "out(" + channel_m.toString() + "," + message_m.toString() + ")." + procNext_m.toString()
  }

  def interprete(env : Map[String,String]) : Unit = {
    
  }
}

case class ProcIf(v : Value, p1 : Proc, p2 : Proc) extends Proc {
  val value_m = v
  val then_m = p1
  val else_m = p2

  override def toString : String = {
    return "if " + value_m.toString + " then " + then_m.toString + " else " + else_m.toString
  }

  def interprete(env : Map[String,String]) : Unit = {
    if (v.interprete(env) != "0")
      p1.interprete(env)
    else
      p2.interprete(env)
  }
}

case class ProcNew(v : ValueConst, p : Proc) extends Proc {
  val value_m = v
  val procNext_m = p

  override def toString : String = {
    return "new(" + value_m.toString + ")." + procNext_m.toString()
  }

  def interprete(env : Map[String,String]) : Unit = {
    val v = value_m.interprete(env);
    p.interprete(env);
  }
}