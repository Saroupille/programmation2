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
  val identifier = s

  override def toString : String = {
	return identifier
  }

  def getValue : Int = {
    return 1
  }
}
object ValueConst {
  def unapply(vc : ValueConst): Option[String] = Some(vc.identifier)
}


// Extractor class for count values
class ValueCount(l : List[Term]) extends Value {
  val list = l

  override def toString : String = {
	def listToString(l : List[Term]) : String =  {
	  l match {
		case head :: tail =>  head.toString + "," + listToString(tail)
		case List() => "0"
	  }
	}
	"count(" + listToString(list) + ")"
  }


  def getValue : Int = {
    return 1
  }
}
object ValueCount {
  def unapply(vc : ValueCount): Option[List[Term]] = Some(vc.list)
}


//Some tests
object Test extends App {

  def printListTest(l : List[Term]) : String = {
	l match {
	  case List() => ""
	  case ValueConst(s) :: tail => {
		"Constant named " + s + "\n" +
		printListTest(tail)
	  }
	  case ValueInteger(i) :: tail => {
		"Integer value = " + i + "\n" +
		printListTest(tail)
	  }
	}
  }

  val termList = List(
	new ValueConst("Const 1"),
	new ValueConst("Const 2"),
	new ValueInteger(15),
	new ValueConst("Const 3"),
	new ValueInteger(30),
	new ValueInteger(40))

  val countTest = new ValueCount(termList)

  //test 1
  println(countTest.toString)

  //test 2
  println(printListTest(termList))

  val publicKey = new TermPublicKey(new ValueInteger(15))
  val privateKey =new TermSecreteKey(new ValueInteger(15))
  val cipher = Cipher.getInstance("RSA")
  cipher.init(Cipher.ENCRYPT_MODE,publicKey.execute)
  val cipherText = cipher.doFinal("test".getBytes())
 
  println(cipherText);
  cipher.init(Cipher.DECRYPT_MODE, privateKey.execute)
  val text=new String(cipher.doFinal(cipherText),"UTF8")
  println(text);
}
