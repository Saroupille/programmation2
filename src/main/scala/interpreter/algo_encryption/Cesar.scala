class CryptoCesar extends CryptoVigenere {
	override def generateKeys(seed : Option[BigInt]) : (VigenerePublicKey, VigenerePrivateKey) = {
	  val gen =
	    seed match {
	      case Some(seed) => Random(seed.longValue())
	      case None => Random()
	    }
	  val randomChar = generator.nextInt(256).toChar;
		(new VigenerePublicKey(randomChar.toString), new VigenerePrivateKey(randomChar.toString));
	}
}
