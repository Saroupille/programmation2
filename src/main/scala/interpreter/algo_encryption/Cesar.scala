class CryptoCesar extends CryptoVigenere {
	override def generateKeys : (VigenerePublicKey, VigenerePrivateKey) = {
		val randomChar = generator.nextInt(256).toChar;
		(new VigenerePublicKey(randomChar.toString), new VigenerePrivateKey(randomChar.toString));
	}
}