import java.io.File


//Class to parse the comand line
case class Config(
debug: Boolean = false,
file: String = "",
synch: Boolean = false
)


object StandardMain {
  def main(args: Array[String]) {
    val parser = new scopt.OptionParser[Config]("CryptIt") {
      head("CryptIt", "0.5")
      opt[Unit]('d', "debug") action {(_,c) =>
        c.copy(debug=true) } text("Debug is a flag. Run some tests before processing")
      opt[Unit]('a', "async") text("Asynchroneous strategy (default)") action { (x, c) =>
        c.copy(synch = false) }
      opt[Unit]('s', "sync") text("Synchroneous strategy") action { (x, c) =>
        c.copy(synch = true) }
      arg[String]("<file>") unbounded() action { (x, c) =>
        c.copy(file = x) } text("File to be interpreted")
      help("help") text("prints this usage text")
    }
    parser.parse(args, Config()) match {
      case Some(cfg) => start(cfg)
      case _ => println("Wrong usage. Use --help for more information")
    }
  }

  def start(config: Config) {  
    if (config.debug) {
      Test.interpretationTest();
      Test.parseTest();
    }
    val interpreter = new Interpreter(config.synch);
    interpreter.interprete(config.file)
  }

}
