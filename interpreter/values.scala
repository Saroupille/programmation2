/**
  * values.scala - Definitions of different case classes to represent a value
  * @author Lanvin Victor Thiré François
  * Copyright (c) 2014 GPLv3. See LICENCE file
  */

import javax.crypto.Cipher;


abstract case class Value() extends Term {
  def getValue : Int
  def interprete : String = return ""
}

// Extractor class for integer values
class ValueInteger(v : Int) extends Value {
  val value_m = v


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
    return 1
  }

  def interprete : String = {
    return identifier_m
  }
}
object ValueConst {
  def unapply(vc : ValueConst): Option[String] = Some(vc.identifier_m)
}


// Extractor class for count values
class ValueCount(l : TermList) extends Value {
  val list_m = l

  override def toString : String = {
  	"count(" + list_m.toString + ")"
  }

  def getValue : Int = { 
    return 1
  }

  def interprete : String = {
    def aux( l:List[Term]) : String = {
      l match {
        case List() => "0"
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
      case TermList(l) => return aux(l)
    }
  }
}
object ValueCount {
  def unapply(vc : ValueCount): Option[TermList] = Some(vc.list_m)
}


// Extractor class for value V > V'
class ValueSuperior(leftv : Value, rightv : Value) extends Value {
  val leftValue_m = leftv
  val rightValue_m = rightv 

  override def toString : String = {
    leftValue_m.toString + " > " + rightValue_m.toString
  }

  def getValue : Int = {
    return 1
  }

  def interprete : String = {
    return if (leftValue_m.interprete.toInt > rightValue_m.interprete.toInt) "1" else "0"
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
    return 1
  }

  def interprete : String = {
    return (leftTerm_m.interprete==rightTerm_m.interprete).toString
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
    return 1
  }

  def interprete : String = {
    return (leftValue_m.interprete!="0" && rightValue_m!="0").toString
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
    return 1
  }

  def interprete : String = {
    return (leftValue_m.interprete!="0" || rightValue_m!="0").toString
  }
}
object ValueOr {
  def unapply(vo : ValueOr): Option[(Value, Value)] = Some((vo.leftValue_m, vo.rightValue_m))
}


// Extractor class for value not(V)
class ValueNot(v : Value) extends Value {
  val value_m = v

  override def toString : String = {
    "not(" + value_m.toString + ")"
  }

  def getValue : Int = {
    return 1
  }

  def interprete : String = {
    return (value_m=="0").toString
  }

 
}
object ValueNot {
  def unapply(vn : ValueNot): Option[Value] = Some(vn.value_m)
}


//Some tests
object Test extends App {

  def testFun (a : Proc) = {
    println(a.toString)
  }

  val parsing = new Parser();

  val parseTest = parsing.parse("out(stdout, count(in(channel, x)::pair(10,15)::[]))|| out(stdout, 10::(15::12::13::[])::(14::13::12/\\11::[])::[])");

  parseTest.foreach(testFun);

  /*
    val publicKey = new TermPublicKey(new ValueInteger(15))
    val privateKey =new TermSecretKey(new ValueInteger(15))
    val cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.ENCRYPT_MODE,publicKey.execute)
    val cipherText = cipher.doFinal("test".getBytes())
   
    println(cipherText);
    cipher.init(Cipher.DECRYPT_MODE, privateKey.execute)
    val text=new String(cipher.doFinal(cipherText),"UTF8")
    println(text);*/
}
