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
}

case class TermValue(v: Value) extends Term {
  val value=v

  override def toString : String = {
    return "Value("+v.toString+")"
  }
}

case class TermVariable(v : String) extends Term {
	val variable = v

	override def toString : String = {
		return "Var("+variable+")"
	}
}

case class TermList(l: List[Term]) extends Term {
  val list=l

  override def toString : String = {
    return "List("+list.toString+")"
  }
}

case class TermPair(t1 : Term, t2 : Term) extends Term {
	val leftTerm_m = t1
	val rightTerm_m = t2

	override def toString : String = {
		return "Pair(" + leftTerm_m.toString() + "," + rightTerm_m.toString() + ")"
	}
}

case class TermProj1(t : Term) extends Term {
  val term_m=t

  override def toString : String = {
    return "Proj1("+t.toString()+")"
  }
}


case class TermProj2(t : Term) extends Term {
  val term_m=t

  override def toString : String = {
    return "Proj2("+t.toString()+")"
  }
}

case class TermEncode(msg : Term, key : Term) extends Term {
  val msg_m=msg
  val key_m=key

  override def toString : String = {
    return "Encode("+msg_m.toString()+","+key_m.toString()+")"
  }
}

case class TermDecode(cypher: Term, key : Term) extends Term {

  val cypher_m=cypher
  val key_m=key

  override def toString : String = {
    return "Decode("+cypher_m.toString()+","+key_m.toString()+")"
  }
}


case class TermPublicKey(seed: Value) extends Term {
  
  val keyGenerator = KeyPairGenerator.getInstance("RSA");
  val secureSeed=SecureRandom.getInstance("SHA1PRNG");
  secureSeed.setSeed(seed.getValue);
  keyGenerator.initialize(2048,secureSeed);
  val keyPair = keyGenerator.generateKeyPair();
  val publicKey=keyPair.getPublic();

  def execute() : java.security.PublicKey = {
    return publicKey
  }
  override def toString : String = {
    return "PublicKey("+publicKey.toString()+")"
  }
}


case class TermSecreteKey(seed: Value) extends Term {
  val keyGenerator = KeyPairGenerator.getInstance("RSA");
  val secureSeed=SecureRandom.getInstance("SHA1PRNG");
  secureSeed.setSeed(seed.getValue);
  keyGenerator.initialize(2048,secureSeed);
  val keyPair = keyGenerator.generateKeyPair();
  val privateKey=keyPair.getPrivate();

  def execute() : java.security.PrivateKey = {
    return privateKey
  }
  override def toString : String = {
    return "SecretKey("+privateKey.toString()+")"
  }
}


