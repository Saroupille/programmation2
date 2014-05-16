import scala.util.Random

trait PublicKey {
	type T
	def getKey: T
}

trait PrivateKey {
	type T
	def getKey: T
}

trait CryptoSystem {
  type PK <: PublicKey
  type SK <: PrivateKey
  val generator = new Random ();
  def encrypt(msg:String, pub:PK, rdm:Int): String
  def decrypt(msg:String, priv:SK): String

  def encrypt(msg:String, pub:String, rdm:Int): String = {
    encrypt(msg, publicKeyFromString(pub),rdm)
  }
  def decrypt(msg:String, priv:String) : String = {
    decrypt(msg, privateKeyFromString(priv))
  }
  def publicKeyToString(pub:PK) : String
  def privateKeyToString(priv:SK) : String
  def publicKeyFromString(pub:String) : PK
  def privateKeyFromString(priv:String) : SK
}
