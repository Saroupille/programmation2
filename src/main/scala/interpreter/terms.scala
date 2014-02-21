/**
  * values.scala - Definitions of different case classes to represent a value
  * @author Lanvin Victor Thiré François
  * Copyright (c) 2014 GPLv3. See LICENCE file
  */

import java.security.KeyPairGenerator;
import java.security.SecureRandom;
import java.nio.ByteBuffer;

abstract class Term() {
  def toString : String
  def interprete : String

  def parseComma(str : String) : Int = {
	//parenthesisCount = 0 ssi on a vu autant de parenthèses ouvrantes que 
    //fermantes (modulo la première ouvrante)

    // Initialisé à -1 car on va parser la parenthèse ouvrante de Pair(
	var parenthesisCount = -1;   
	var i = 0;
	while(parenthesisCount != 0 || str.charAt(i) != ',') {
  	  if (str.charAt(i) == '(')
  	    parenthesisCount += 1;
  	  else if (str.charAt(i) == ')')
  	    parenthesisCount -= 1;
  	  i += 1;
  	}
  	return i
  }
}

case class TermVariable(v : String) extends Term {
  val variable_m = v

  override def toString : String = {
	return "Var("+variable_m+")"
  }

  def interprete : String = {
    return variable_m
  }
}

case class TermList(l: List[Term]) extends Term {
  val list_m=l

  override def toString : String = {
    def listToString(l : List[Term]) : String =  {
      l match {
        case head :: tail =>  head.toString + "," + listToString(tail)
        case List() => "0"
      }
    }
    "[" + listToString(list_m) + "]"
  }

  def interprete : String = {
    def aux(l : List[Term]) : String = {
      l match {
        case List() => "[]"
        case head :: tail =>  head.interprete + "::" + aux(tail)
      }
    }
    return aux(list_m)
  }
}

case class TermPair(t1 : Term, t2 : Term) extends Term {
  val leftTerm_m = t1
  val rightTerm_m = t2

  override def toString : String = {
	  return "Pair(" + leftTerm_m.toString() + "," + rightTerm_m.toString() + ")"
  }

  def interprete : String = {
    return "pair("+leftTerm_m.interprete+","+rightTerm_m.interprete+")"
  }
}

case class TermProj1(t : Term) extends Term {
  val term_m=t

  override def toString : String = {
    return "Proj1("+t.toString()+")"
  }

  def interprete : String = {
    val tmp = term_m.interprete
    if(tmp.startsWith("pair("))
      return tmp.substring(5,parseComma(tmp))
    else
      return "err"
  }
}


case class TermProj2(t : Term) extends Term {
  val term_m=t

  override def toString : String = {
    return "Proj2("+t.toString()+")"
  }

  def interprete : String = {
    val tmp = term_m.interprete
    if(tmp.startsWith("pair(")) 
      return tmp.substring(parseComma(tmp)+1,tmp.lastIndexOf(")"))
    else
      return "err"   
  }
}


case class TermEncode(msg : Term, key : Term) extends Term {
  val msg_m=msg
  val key_m=key

  override def toString : String = {
    return "Encode("+msg_m.toString()+","+key_m.toString()+")"
  }

  def interprete : String = {
    return "enc("+msg_m.interprete+","+key_m.interprete+")"
  }
}

case class TermDecode(cypher: Term, key : Term) extends Term {

  val cypher_m=cypher
  val key_m=key

  override def toString : String = {
    return "Decode("+cypher_m.toString()+","+key_m.toString()+")"
  }
  
  def interprete : String = {
    val u=cypher.interprete
    if(u.startsWith("enc(")) { 
      val u1=u.substring(4,parseComma(u))
      val pk=u.substring(parseComma(u)+1,u.lastIndexOf(")"))
      val sk=key_m.interprete
      if(pk.startsWith("pk(")) {
        val n = pk.substring(3,pk.lastIndexOf(")"))
        if(sk.startsWith("sk(")) {
          val n2 = sk.substring(3,sk.lastIndexOf(")"))
          try {
            n.toInt
          } catch {
            case e:Exception=>{
              return "err"
            }
          }

          if(n==n2)
            return u1
        }
      }
    }
    return "err"
    
  }
}


case class TermPublicKey(v: Value) extends Term {
  val publicKey_m=v
  /*
  val publicKey_m={
    var seed=0
    v match {
      case ValueInteger(n) => seed=n
      case _ => throw new noIntegerPublicKey("bouh")
    }
    val keyGenerator = KeyPairGenerator.getInstance("RSA");
    /*with SHA1PRNG, generation of keys is deterministic.
     It is not secured.*/
    val secureSeed=SecureRandom.getInstance("SHA1PRNG");
    secureSeed.setSeed(seed);
    keyGenerator.initialize(2048,secureSeed);
    val keyPair = keyGenerator.generateKeyPair();
    keyPair.getPublic();
  }

  def execute() : java.security.PublicKey = {
    return publicKey_m
  }
   */
  override def toString : String = {
    return "PublicKey("+publicKey_m.toString()+")"
  }

  def interprete : String = {
    try {
      return "pk("+publicKey_m.interprete+")"
    } catch {
      case e:Exception=>{
        return "err"
      }
    }
    return "err"
  }
}


case class TermSecretKey(v: Value) extends Term {
  val privateKey_m=v
  /*
  val privateKey_m={ //See TermPublicKey
    var seed=0
    v match {
      case ValueInteger(n) => seed=n
      case _ => throw new noIntegerSecretKey("bouh")
    }
    val keyGenerator = KeyPairGenerator.getInstance("RSA");
    val secureSeed=SecureRandom.getInstance("SHA1PRNG");
    secureSeed.setSeed(seed);
    keyGenerator.initialize(2048,secureSeed);
    val keyPair = keyGenerator.generateKeyPair();
    keyPair.getPrivate();
  }

  def execute() : java.security.PrivateKey = {
    return privateKey_m
  }
  */

  def interprete : String = {
    try {
      return "sk("+privateKey_m.interprete.toInt+")"
    } catch {
      case e:Exception=>{
        return "err"
      }
    }
    return "err"
  }
  override def toString : String = {
    return "SecretKey("+privateKey_m.toString()+")"
  }
}


