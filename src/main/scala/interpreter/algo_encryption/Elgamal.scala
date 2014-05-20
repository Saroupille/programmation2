import scala.util.Random

class CryptoElGamal[E] (group : Group[E]) extends CryptoSystem {
	class ElGamalPublicKey (init : E) extends PublicKey {
		type T = E
		val key = init
		def getKey : T = key
	}

	class ElGamalPrivateKey (init : BigInt) extends PrivateKey {
		type T = BigInt
		val key = init
		def getKey : T = key 
	}

	type PK = ElGamalPublicKey
	type SK = ElGamalPrivateKey

	def generateKeys(seed : Option[BigInt]) : (ElGamalPublicKey, ElGamalPrivateKey) = {
	  val gen =
	    seed match {
	      case Some(seed) => new Random(seed.longValue())
	      case None => new Random()
	    }
    //val gen=Ran

		val randomInt = BigInt(1000, gen)%(group.order-1) + 1;
		val expoElt = group.exp(group.generator, randomInt);
		(new ElGamalPublicKey(expoElt), new ElGamalPrivateKey(randomInt));
	}

	def encrypt(msg:String, pub:PK, rdm:Int): String = {
		val gen = new Random(rdm);
		val randomInt = BigInt(1000, gen)%(group.order-1) + 1;
		val expoElt = group.exp(group.generator, randomInt);
		val sharedSecret = group.exp(pub.getKey, randomInt);
		def injectString : E = {
			var stringCode : BigInt = 0;
			for (i <- 0 to msg.length - 1) {
				stringCode = stringCode * 256 + msg(i).toInt
			}
			return group.fromBigInt(stringCode)
		}
		val cryptedMsg = group.combines(injectString, sharedSecret);

		return (group.toString(expoElt) + "#" + group.toString(cryptedMsg) + "#" + publicKeyToString(pub))
	}

 	def decrypt(msg:String, priv:SK): String = {
 		def cutString : (E,E) = {
 			val i = msg.indexOf("#");
 			(group.fromString (msg.substring(0, i)), group.fromString (msg.substring(i+1, msg.lastIndexOf("#"))))
 		}
 		val (e1, e2) = cutString;
 		val sharedSecret = group.exp(e1, group.order - priv.getKey);
 		val cryptedMsg = group.toBigInt (group.combines(e2, sharedSecret));
 		def decryptString : String = {
 			val length = cryptedMsg.bitLength / 8 + 1;
 			var actualMsg : BigInt = cryptedMsg;
 			var chars : Array[Char] = new Array[Char](length);
 			for (i <- 1 to length) {
 				chars(length-i) = (actualMsg % 256).toChar;
 				actualMsg = actualMsg / 256;
 			}
 			return chars.mkString
 		}
 		return decryptString	
	}

	def publicKeyToString(pub:PK) : String = {
		group.toString(pub.getKey) + ";" + group.order.toString
	}

  	def privateKeyToString(priv:SK) : String = {
  		group.toString(group.fromBigInt(priv.getKey))
  	}

  	def publicKeyFromString(pub:String) : PK = {
  		new ElGamalPublicKey (group.fromString(pub.substring(0, pub.lastIndexOf(";"))))
  	}

  	def privateKeyFromString(priv:String) : SK = {
  		new ElGamalPrivateKey (group.toBigInt(group.fromString(priv)))
  	}
}
