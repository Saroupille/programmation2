import scala.util.Random

class CryptoVigenere extends CryptoSystem {
	class VigenerePublicKey (init : String) extends PublicKey {
		type T = String
		val key = init
		def getKey : T = key
	}

	class VigenerePrivateKey (init : String) extends PrivateKey {
		type T = String
		val key = init
		def getKey : T = key 
	}

	type PK = VigenerePublicKey
	type SK = VigenerePrivateKey

	def generateKeys(seed : Option[BigInt]) : (VigenerePublicKey, VigenerePrivateKey) = {
	  val gen =
	    seed match {
	      case Some(seed) => new Random(seed.longValue())
	      case None => new Random()
	    }
    //val gen=Ran

		def generateString (length : Int) : String = {
			var chars = new Array[Char](length);
			for (i <- 0 to length - 1) {
				chars(i) = gen.nextInt(256).toChar;
			}
			chars.mkString;
		}
		val randomString = generateString(gen.nextInt(5) + 5);
		(new VigenerePublicKey(randomString), new VigenerePrivateKey(randomString));
	}

	def slide(a : Char, n : Int) : Char = {
		(((a.toInt + n) + 256)%256).toChar
	}

	def encrypt(msg:String, pub:PK, rdm:Int): String = {
		var charlist : List[Char] = List();
		val key = pub.getKey;
		val max = key.length;
		for (i <- 0 to msg.length - 1){
			val sl = key.charAt(i%max).toInt;
			charlist = slide(msg.charAt(i), sl) :: charlist
		}
		charlist.reverse.mkString
	}

 	def decrypt(msg:String, priv:SK): String = {
		var charlist : List[Char] = List();
		val key = priv.getKey;
		val max = key.length;
		for (i <- 0 to msg.length - 1){
			val sl = key.charAt(i%max).toInt;
			charlist = slide(msg.charAt(i), -sl) :: charlist
		}
		charlist.reverse.mkString
	}


  override def publicKeyToString(pub:PK) : String = {
    pub.getKey
  }
  override def privateKeyToString(priv:SK) : String = {
    priv.getKey
  }
  override def publicKeyFromString(pub:String) : PK = {
    new VigenerePublicKey(pub)
  }
  override def privateKeyFromString(priv:String) : SK = {
    new VigenerePrivateKey(priv)
  }
}
