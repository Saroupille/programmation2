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
}
