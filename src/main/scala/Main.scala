/**
* Main.scala - Principal class, options parsing
* @author Lanvin Victor Thiré François
* Copyright (c) 2014 GPLv3. See LICENCE file
*/

import java.io.File


//Class to parse the command line
case class Config(
debug: Boolean = false,
file: String = "",
synch: Boolean = false,
enc_algo: String ="",
keysize: Int = 1024,
zpadd: Boolean = false,
zpmul: Boolean = true,
ec: Boolean = false
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
      arg[Int]("keysize") optional() action { (x,c) =>
        c.copy(keysize=x) } text("Size of the key if the algorithm is RSA (default 1024)") 
      arg[Unit]("zpadd") optional() action { (x,c) =>
        c.copy(zpadd=true,zpmul=false,ec=false) } text("Use el gama with (Z,+). Give a prime number") 
      arg[Unit]("zpmul") optional() action { (x,c) =>
        c.copy(zpmul=true,zpadd=false,ec=false) } text("Use el gama with (Z,x). Give a prime number (default)") 
      opt[Unit]("ec") optional() action { (x,c) =>
        c.copy(ec=true,zpadd=false,zpmul=false) } text("Use el gamal with elliptic curves") 
      opt[Unit]("RSA") optional() action { (x,c) =>
        c.copy(enc_algo="RSA") } text("Use RSA algorithm")
      opt[Unit]("Vigenere") optional() action { (x,c) =>
        c.copy(enc_algo="Vigenere") } text("Use vigenere algorithm")
      opt[Unit]("Cesar") optional() action { (x,c) =>
        c.copy(enc_algo="Cesar") } text("Use cesar algorithm")
      opt[Unit]("Elgamal") optional() action { (x,c) =>
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
    val interpreter = new Interpreter(config.synch);
    println(config.file);
    interpreter.interprete(config.file)
  }

}
