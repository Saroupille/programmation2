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
case class TermEncode(msg_m : Term, key_m : Term) extends Term {

  def interprete(env : Map[String,String]) : String = {
    return "enc("+msg_m.interprete(env)+","+key_m.interprete(env)+")"
  }
}


//Dec(T,k)
case class TermDecode(cypher_m: Term, key_m : Term) extends Term {
  //Re-parse the string representing the term cypher_m
  def interprete(env : Map[String,String]) : String = {
    val u=cypher_m.interprete(env)
    if(u.startsWith("enc(")) { 
      val u1=u.substring(4,Term.parseStrPar(u, ",", -1))
      val pk=u.substring(Term.parseStrPar(u, ",", -1)+1,u.lastIndexOf(")"))
      val sk=key_m.interprete(env)
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



//Pk(v)
case class TermPublicKey(publicKey_m: Value) extends Term {
  //Real key-generating code
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

  def interprete(env : Map[String,String]) : String = {
    try {
      return "pk("+publicKey_m.interprete(env)+")"
    } catch {
      case e:Exception=>{
        return "err"
      }
    }
    return "err"
  }
}


//Sk(v)
case class TermSecretKey(privateKey_m: Value) extends Term {

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

  def interprete(env : Map[String,String]) : String = {
    try {
      return "sk("+privateKey_m.interprete(env).toInt+")"
    } catch {
      case e:Exception=>{
        return "err"
      }
    }
    return "err"
  }
}


