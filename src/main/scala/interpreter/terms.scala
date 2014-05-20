/**
  * terms.scala - Definition of different case classes to represent a term
  * @author Lanvin Victor Thiré François
  * Copyright (c) 2014 GPLv3. See LICENCE file
  */

//import java.security.KeyPairGenerator;
//import java.security.SecureRandom;
import java.nio.ByteBuffer;
import scala.collection.mutable.Map

abstract class Term() {

  def interprete(env : Map[String,String]) : String
}
object Term {
  
  var cs :Option[CryptoSystem] = None  
  
  def getCryptoSystem() = {
    cs match {
      case None => throw new Exception("whatever")
      case Some(thing) => thing //haha
    }
  }
  def setCryptoSystem(param:CryptoSystem) = {
    cs match {
      case None => cs=Some(param)
        /* TO DO : define exception */
      case Some(_) => ()
    }
  }
  //Function used in the parser, return the first ocurrence of the string 'find' in 'str' that
  //is inside -init parenthesis

  def parseStrPar(str : String, find : String, init : Int) : Int = {
    var parenthesisCount = init;
    var n = str.length();
    var i = 0;
    while((parenthesisCount != 0 || !str.substring(i).startsWith(find)) && i < n) {
      if (str.charAt(i) == '(')
        parenthesisCount += 1;
      else if (str.charAt(i) == ')')
        parenthesisCount -= 1;
      i += 1;
    }
    if (i < n)
      return i;
    return -1;
  }
}


//Variable
case class TermVariable(variable_m : String) extends Term {

  def interprete(env : Map[String,String]) : String = {
    try {
      return env(variable_m);
    }
    catch {//The variable is not in the environnement, we add the corresponding string
      case _ : Throwable =>
        env.put(variable_m, variable_m);
        return variable_m
    }
  }
}



//List
case class TermList(list_m: List[Term]) extends Term {

  def interprete(env : Map[String,String]) : String = {
    def aux(l : List[Term]) : String = {
      l match {
        case List() => "[]"
        case head :: tail =>  head.interprete(env) + "::" + aux(tail)
      }
    }
    return aux(list_m)
  }
}



//Pair(a,b)
case class TermPair(leftTerm_m : Term, rightTerm_m : Term) extends Term {
  
  def interprete(env : Map[String,String]) : String = {
    return "pair("+leftTerm_m.interprete(env)+","+rightTerm_m.interprete(env)+")"
  }
}



//Proj1(T)
case class TermProj1(term_m : Term) extends Term {

  def interprete(env : Map[String,String]) : String = {
    val tmp = term_m.interprete(env)
    if(tmp.startsWith("pair("))
      return tmp.substring(5,Term.parseStrPar(tmp, ",", -1))
    else
      return "err"
  }
}


//Proj2(T)
case class TermProj2(term_m : Term) extends Term {

  def interprete(env : Map[String,String]) : String = {
    val tmp = term_m.interprete(env)
    if(tmp.startsWith("pair("))
      return tmp.substring(Term.parseStrPar(tmp, ",", -1)+1,tmp.lastIndexOf(")"))
    else
      return "err"
  }
}


//Enc(m,k)
case class TermEncode(msg_m : Term, key_m : Term, rdm : Term) extends Term {
  def interprete(env : Map[String, String]) : String = {
    return "enc("+Term.getCryptoSystem.encrypt(msg_m.interprete(env), key_m.interprete(env),rdm.interprete(env).toInt)+")"
  }
}


//Dec(T,k)
case class TermDecode(cypher_m: Term, key_m : Term) extends Term {
  def interprete(env : Map[String,String]) : String = { 
    val u=cypher_m.interprete(env)
    var msg=""
    if(u.startsWith("enc(")) {
      msg = u.substring(4,u.lastIndexOf(")"))

    }
    else {
      throw new Exception("Wrong decode msg")
    }
    return "D("+Term.getCryptoSystem.decrypt(msg, key_m.interprete(env))+")"
  }
}



//Pk(v)
case class TermPublicKey(seed_s: Value) extends Term {
  def interprete(env : Map[String, String]) : String = {
    val cs = Term.getCryptoSystem()
    val seed =BigInt.apply(seed_s.interprete(env))
    cs.publicKeyToString(cs.generateKeys(Some(seed))._1)

  }
}


//Sk(v)
case class TermSecretKey(seed_s: Value) extends Term {
  def interprete(env : Map[String,String]) : String = {
    val cs = Term.getCryptoSystem()
    val seed =BigInt.apply(seed_s.interprete(env))
    cs.privateKeyToString(cs.generateKeys(Some(seed))._2)
  }
}


