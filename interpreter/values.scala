/**
  * values.scala - Definitions of different case classes to represent a value
  * @author Lanvin Victor Thiré François
  * Copyright (c) 2014 GPLv3. See LICENCE file
  */

import javax.crypto.Cipher;


abstract case class Value() extends Term {
  def getValue : Int
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
}
object ValueSuperior {
  def unapply(vs : ValueSuperior): Option[(Value, Value)] = Some((vs.leftValue_m, vs.rightValue_m))
}


// Extractor class for value V = V'
class ValueEqual(leftv : Value, rightv : Value) extends Value {
  val leftValue_m = leftv
  val rightValue_m = rightv 

  override def toString : String = {
    leftValue_m.toString + " = " + rightValue_m.toString
  }

  def getValue : Int = {
    return 1
  }
}
object ValueEqual {
  def unapply(ve : ValueEqual): Option[(Value, Value)] = Some((ve.leftValue_m, ve.rightValue_m))
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
}
object ValueOr {
  def unapply(vo : ValueOr): Option[(Value, Value)] = Some((vo.leftValue_m, vo.rightValue_m))
}


// Extractor class for value not(V)
class ValueNot(v : Value) extends Value {
  val argValue_m = v

  override def toString : String = {
    "not(" + argValue_m.toString + ")"
  }

  def getValue : Int = {
    return 1
  }
}
object ValueNot {
  def unapply(vn : ValueNot): Option[Value] = Some(vn.argValue_m)
}


//Some tests
object Test extends App {

  def printValueTest(v : Value) : String = {
    v match {
      case ValueConst(s) => "Constant named " + s + "\n"
      case ValueInteger(i) => "Integer value = " + i + "\n"
      case ValueSuperior(v1, v2) => "Superior value : " + v1.toString + " > " + v2.toString
    }
  }

  def printListTest(l : List[Term]) : String = {
  	l match {
  	  case List() => ""
  	  case TermVariable(s) :: tail => {
  		  "Variable named " + s + "\n" +
  		  printListTest(tail)
  	  }
  	  case (v : Value) :: tail => {
  		  printValueTest(v) + printListTest(tail)
  	  }
  	}
  }

  val termList = new TermList(List(
  new TermVariable("Var1"),
	new ValueConst("Const 1"),
	new ValueConst("Const 2"),
	new ValueInteger(15),
	new ValueConst("Const 3"),
	new ValueInteger(30),
	new ValueInteger(40),
  new ValueSuperior(new ValueInteger(40), new ValueInteger(50))))

  val countTest = new ValueCount(termList)

  //test 1
  println(countTest.toString)

  //test 2
  println(printListTest(termList.list))

  val publicKey = new TermPublicKey(new ValueInteger(15))
  val privateKey =new TermSecretKey(new ValueInteger(15))
  val cipher = Cipher.getInstance("RSA")
  cipher.init(Cipher.ENCRYPT_MODE,publicKey.execute)
  val cipherText = cipher.doFinal("test".getBytes())
 
  println(cipherText);
  cipher.init(Cipher.DECRYPT_MODE, privateKey.execute)
  val text=new String(cipher.doFinal(cipherText),"UTF8")
  println(text);
}
