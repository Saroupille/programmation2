/**
  * termhack.scala - Definition of the openEnc term
  * @author Lanvin Victor Thiré François
  * Copyright (c) 2014 GPLv3. See LICENCE file
  */

import java.nio.ByteBuffer;
import scala.collection.mutable.Map

case class TermOpenEncode(mainTerm : Term) extends Term {
    val french_distribution = Map(
      ('e', 0.14715),
      ('s', 0.07948),
      ('a', 0.07636),
      ('i', 0.07529),
      ('t', 0.07244),
      ('n', 0.07095),
      ('r', 0.06553),
      ('u', 0.06311),
      ('l', 0.05456),
      ('o', 0.05378),
      ('d', 0.03669),
      ('c', 0.03260),
      ('p', 0.03021),
      ('m', 0.02968),
      ('é', 0.01904),
      ('v', 0.01628),
      ('q', 0.01362),
      ('f', 0.01066),
      ('b', 0.00901),
      ('g', 0.00866),
      ('h', 0.00737),
      ('j', 0.00545),
      ('à', 0.00486),
      ('x', 0.00387),
      ('y', 0.00308),
      ('è', 0.00271),
      ('ê', 0.00225),
      ('z', 0.00136),
      ('w', 0.00114),
      ('ç', 0.00085),
      ('ù', 0.00058),
      ('k', 0.00049),
      ('î', 0.00045)
    )

    def distance_euclidienne(text:String):Double = {
      val text_clean=text.toLowerCase().stripLineEnd.replaceAll(" ", "");
      var distance = 0.
      val frequency = text_clean.groupBy(_.toChar).map{ p => (p._1, (p._2.length.toDouble/text_clean.length))}
      
      frequency.foreach((p: (Char,Double)) => 
        french_distribution.get(p._1) match {
          case None => distance+=p._2*p._2
          case Some(m) => distance+=math.abs(p._2-m)*math.abs(p._2-m)
        })
      return math.sqrt(distance)
    }

    def decryptCesar(cipher:String): (String,Int, Double) = {
      val min= 48
      val max= 256
      val cesar=new CryptoCesar
      var d=0.
      var attempt=""
      var min_d=0.
      var min_i=0
      for(i<-min to  max) {

        attempt=cesar.decrypt(cipher, i.toChar.toString)
        d=distance_euclidienne(attempt)
        if (min_d == 0. || d < min_d) {
          
          min_d=d
          min_i=i
        }
      }
      (cesar.decrypt(cipher,min_i.toChar.toString),min_i,min_d)
    }

    def my_sliding(text:String ,i:Int): Array[String] = {
      var res=new Array[String](i)
      for(j<-0 until i) {
        res(j)=""
      }
      var cpt=0
      for(j<-0 until text.length) {
        res(cpt)+=text(j)
        cpt=(cpt+1)%i
      }
      res
    }

    def decryptVigenere(cipher:String) : String = {
      val keysize_min=1
      val keysize_max=math.sqrt(cipher.length).toInt
      var dist=0.
      var dt=0.
      var min_dist=0.
      var min_key=new Array[Int](keysize_max)
      var key_found=""
      var tuple=("",0,0.)
      val key=new Array[Int](keysize_max)
      
      for(i<-0 until keysize_max) {
        key(i)= -1
      }

      for(i<-keysize_min until keysize_max) {
        val cesars=my_sliding(cipher,i)
        for(j<-0 until i) {
          tuple=decryptCesar(cesars(j))
          key(j)=tuple._2
          dt=tuple._3
          dist+=dt
        }
        dist/=i
        if(min_dist== 0. || dist< min_dist) {
          min_dist=dist
          min_key=key.clone()
          
        }
        dist=0;
        for(j<-0 until keysize_max) {
          key(j)= -1
        } 
      }
      for(i<-0 until keysize_max) {
        if(min_key(i)!= -1) {
          
          key_found+=min_key(i).toChar.toString
        }
      }
      val vigenere = new CryptoVigenere
      vigenere.decrypt(cipher,key_found)
    }

  def interprete(env : Map[String,String]) : String = {
    val strDec = mainTerm.interprete(env);
    var msg=""
    if(strDec.startsWith("enc(")) {
      msg = strDec.substring(4,strDec.lastIndexOf(")"))

    }
    Term.cs match {
      case Some(csys) => {
        csys match {
          case _ : CryptoCesar => decryptCesar(msg)._1
          case _ : CryptoVigenere => decryptVigenere(msg)
          case _ => strDec
        }
      }
      case _ => strDec
    }
  }
}
