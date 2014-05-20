/**
* Main.scala - Principal class, options parsing
* @author Lanvin Victor Thiré François
* Copyright (c) 2014 GPLv3. See LICENCE file
*/

import java.io.File

case class configError(s: String) extends Exception(s)


//Class to parse the command line
case class Config(
debug: Boolean = false,
file: String = "",
synch: Boolean = false,
enc_algo: String ="",
keysize: Int = 1024,
zpadd: Boolean = false,
zpmul: Boolean = true,
ec: Boolean = false,
prime: BigInt = BigInt("531872289054204184185084734375133399408303613982130856645299464930952178606045848877129147820387996428175564228204785846141207532462936339834139412401975338705794646595487324365194792822189473092273993580587964571659678084484152603881094176995594813302284232006001752128168901293560051833646881436219")
)


object StandardMain {
  def main(args: Array[String]) {

    val parser = new scopt.OptionParser[Config]("CryptIt") {
      head("CryptIt", "3.14159")
      opt[Unit]('d', "debug") action {(_,c) =>
        c.copy(debug=true) } text("Debug is a flag. Run some tests before processing")
      opt[Unit]('a', "async") text("Asynchroneous strategy (default)") action { (x, c) =>
        c.copy(synch = false)  }
      opt[Unit]('s', "sync") text("Synchroneous strategy") action { (x, c) =>
        c.copy(synch = true) }
      arg[String]("<file>") required() action { (x, c) =>
        c.copy(file = x) } text("File to be interpreted")
      opt[Int]("keysize") optional() action { (x,c) =>
        c.copy(keysize=x) } text("Size of the key if the algorithm is RSA (default 1024)") 
      opt[BigInt]("zpadd") optional() action { (x,c) =>
        if (!x.isProbablePrime(5))
          throw configError("\nThis is not a prime number !");
        c.copy(prime=x, zpadd=true,zpmul=false,ec=false) } text("Use el gamal with (Z,+). Take a prime number as parameter.") 
      opt[BigInt]("zpmul") optional() action { (x,c) =>
        if (!x.isProbablePrime(5))
          throw configError("\nThis is not a prime number !");
        c.copy(prime=x, zpmul=true,zpadd=false,ec=false) } text("Use el gamal with (Z,x). Take a prime number as parameter.") 
      opt[Unit]("ec") optional() action { (x,c) =>
        c.copy(ec=true,zpadd=false,zpmul=false) } text("Use el gamal with elliptic curves") 
      opt[Unit]("RSA") optional() action { (x,c) =>
        c.copy(enc_algo="RSA") } text("Use RSA algorithm")
      opt[Unit]("Vigenere") optional() action { (x,c) =>
        c.copy(enc_algo="Vigenere") } text("Use vigenere algorithm")
      opt[Unit]("Cesar") optional() action { (x,c) =>
        c.copy(enc_algo="Cesar") } text("Use cesar algorithm")
      opt[Unit]("elGamal") optional() action { (x,c) =>
        c.copy(enc_algo="Elgamal") } text("Use el gamal algorithm")
      help("help") text("prints this usage text")
      checkConfig { c => 
        if (c.enc_algo=="") failure("You have to precise an algorithm like RSA or cesar") else success}
    }
    parser.parse(args, Config()) match {
      case Some(cfg) => start(cfg)
      case _ => println("Wrong usage. Use --help for more information")
    }
  }

  def start(config: Config) {  
    if (config.debug) {           // Launch some tests if the flag debug is used
      Test.interpretationTest();
      Test.parseTest();
    }

    RSA.setBytes(config.keysize);

    val cs : CryptoSystem = 
      if (config.enc_algo == "RSA") 
        new RSA 
      else if (config.enc_algo == "Cesar")
        new CryptoCesar
      else if (config.enc_algo == "Vigenere")
        new CryptoVigenere
      else if (config.enc_algo == "Elgamal") {
        if (config.zpadd)
          new CryptoElGamal(new Zadd(config.prime))
        else if (config.zpmul)
          new CryptoElGamal(new Zmult(config.prime))
        else 
          throw configError("Unknown group")
      }
      else 
        throw configError("Unknown cryptosystem");

    val interpreter = new Interpreter(config.synch, cs);
    println(config.file);
    interpreter.interprete(config.file)
  }

}
