case class noIntegerPublicKey(s: String) extends Exception(s)

case class noIntegerSecretKey(s: String) extends Exception(s)

case class parsingError(s: String) extends Exception(s)

case class interpretationError(s: String) extends Exception(s)