/**
  * values.scala - Definitions of different case classes to represent a value
  * @author Lanvin Victor Thiré François
  * Copyright (c) 2014 GPLv3. See LICENCE file
  */
  
import util.Random;


abstract case class Value() extends Term {
  var value_m : Int = -1
  def getValue : Int
}

// Extractor class for integer values
class ValueInteger(v : Int) extends Value {
  value_m = v


  override def toString : String = {
   return value_m.toString
  }

  def getValue : Int = {
    return value_m
  }

  def interprete : String = {
    return value_m.toString
  }
}
object ValueInteger {
  def unapply(vi : ValueInteger): Option[Int] = Some(vi.value_m)
}


// Extractor class for constant values
class ValueConst(s : String) extends Value {
  val identifier_m = s

  override def toString : String = {
   return identifier_m
  }

  def getValue : Int = {
    return value_m
  }

  def interprete : String = {
    if(value_m == -1) {
      val rand = new Random();
      value_m = rand.nextInt(100000000);
    }
    return value_m.toString;
  }
}
object ValueConst {
  def unapply(vc : ValueConst): Option[String] = Some(vc.identifier_m)
}


// Extractor class for count values
class ValueCount(l : Term) extends Value {
  val list_m = l

  override def toString : String = {
    "count(" + list_m.toString + ")"
  }

  def getValue : Int = { 
    return value_m
  }

  def interprete : String = {
    def aux( l:List[Term]) : Int = {
      l match {
        case List() => 0
        case head::tail => try {
          val h = head.interprete.toInt
          if (h==0)
            aux(tail)
          else
            1+aux(tail)
        } catch {
          case e:Exception=>{
            aux(tail)
          }
        }
      }
    }
    list_m match {
      case TermList(l) => 
        value_m = aux(l); 
        return value_m.toString
    }
  }
}
object ValueCount {
  def unapply(vc : ValueCount): Option[Term] = Some(vc.list_m)
}


// Extractor class for value V > V'
class ValueSuperior(leftv : Value, rightv : Value) extends Value {
  val leftValue_m = leftv
  val rightValue_m = rightv 

  override def toString : String = {
    leftValue_m.toString + " > " + rightValue_m.toString
  }

  def getValue : Int = {
    return value_m
  }

  def interprete : String = {
    if (leftValue_m.interprete.toInt > rightValue_m.interprete.toInt) value_m = 1 else value_m = 0
    return value_m.toString
  }
}
object ValueSuperior {
  def unapply(vs : ValueSuperior): Option[(Value, Value)] = Some((vs.leftValue_m, vs.rightValue_m))
}


// Extractor class for value V = V'
class ValueEqual(leftv : Term, rightv : Term) extends Value {
  val leftTerm_m = leftv
  val rightTerm_m = rightv 

  override def toString : String = {
    leftTerm_m.toString + " = " + rightTerm_m.toString
  }

  def getValue : Int = {
    return value_m
  }

  def interprete : String = {
    value_m = if (leftTerm_m.interprete==rightTerm_m.interprete) 1 else 0
    return value_m.toString
  }
}
object ValueEqual {
  def unapply(ve : ValueEqual): Option[(Term, Term)] = Some((ve.leftTerm_m, ve.rightTerm_m))
}


// Extractor class for value V /\ V'
class ValueAnd(leftv : Value, rightv : Value) extends Value {
  val leftValue_m = leftv
  val rightValue_m = rightv 

  override def toString : String = {
    leftValue_m.toString + " /\\ " + rightValue_m.toString
  }

  def getValue : Int = {
    return value_m
  }

  def interprete : String = {
    value_m = if (leftValue_m.interprete!="0" && rightValue_m.interprete!="0") 1 else 0
    return value_m.toString
  }
}
object ValueAnd {
  def unapply(va : ValueAnd): Option[(Value, Value)] = Some((va.leftValue_m, va.rightValue_m))
}


// Extractor class for value V \/ V'
class ValueOr(leftv : Value, rightv : Value) extends Value {
  val leftValue_m = leftv
  val rightValue_m = rightv 

  override def toString : String = {
    leftValue_m.toString + " \\/ " + rightValue_m.toString
  }

  def getValue : Int = {
    return value_m
  }

  def interprete : String = {
    value_m = if (leftValue_m.interprete!="0" || rightValue_m.interprete!="0") 1 else 0
    return value_m.toString
  }
}
object ValueOr {
  def unapply(vo : ValueOr): Option[(Value, Value)] = Some((vo.leftValue_m, vo.rightValue_m))
}


// Extractor class for value not(V)
class ValueNot(v : Value) extends Value {
  val valuenot_m = v

  override def toString : String = {
    "not(" + valuenot_m.toString + ")"
  }

  def getValue : Int = {
    return value_m
  }

  def interprete : String = {
    value_m = if (valuenot_m.interprete=="0") 1 else 0
    return value_m.toString
  }

 
}
object ValueNot {
  def unapply(vn : ValueNot): Option[Value] = Some(vn.valuenot_m)
}
