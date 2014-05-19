import scala.util.Random

class RSA extends CryptoSystem {

  class RSAPublicKey(n:BigInt,e:BigInt) extends PublicKey {
    type T = (BigInt,BigInt)
    val key = (n,e)
    def getKey : T = key
  }
  class RSAPrivateKey(n:BigInt, d:BigInt) extends PrivateKey {
    type T = (BigInt,BigInt)
    val key = (n,d)
    def getKey : T = key
  }

  type PK = RSAPublicKey
  type SK = RSAPrivateKey

  
  
  def generateKeys(seed : Option[BigInt]): (RSAPublicKey, RSAPrivateKey) = {
    val gen =
      seed match {
        case Some(seed) => new Random(seed.longValue())
        case None => new Random()
      }
    //val gen=Random
    val p = BigInt.probablePrime(RSA.bytes, gen)
    val q = BigInt.probablePrime(RSA.bytes, gen)
    val n = p*q
    val one = BigInt(1)
    val m = (p-1)*(q-1)
    var e = BigInt(65537)
    while(m.gcd(e)>1)
      e=e+BigInt(2)

    val d=e.modInverse(m)
    return (new RSAPublicKey(n,e),new RSAPrivateKey(n,d))
  }

  //java.nio.charset.Charset.forName("ISO-8859-1")
  def encrypt(msg:String, key:PK, rdm:Int): String = {
    val bytesText=msg.getBytes()
    val chunk=RSA.bytes/4
    val length=bytesText.length
    val nbchunks=(length-1)/chunk

    val e = key.getKey._2
    val n = key.getKey._1
    var cypher=""
    for(i<-0 to nbchunks) {
      val bigIntText=BigInt.apply(bytesText.slice(chunk*i, chunk*(i+1)))
      cypher+=new String(bigIntText.modPow(e,n).toByteArray, java.nio.charset.Charset.forName("ISO-8859-1"))
    }
    return cypher
  }
  def decrypt(msg:String, key:SK): String = {
    val bytesText=msg.getBytes(java.nio.charset.Charset.forName("ISO-8859-1"))
    val chunk=RSA.bytes/4
    val length=bytesText.length
    val nbchunks=(length-1)/chunk
    var decrypt=""
    val d = key.getKey._2
    val n = key.getKey._1
    
    for(i<-0 to nbchunks) {
      val bigIntText=BigInt.apply(bytesText.slice(chunk*i, chunk*(i+1)))
      decrypt+=new String(bigIntText.modPow(d,n).toByteArray)
    }
    
    return decrypt
  }
  def publicKeyToString(pub:PK) : String = {
    val (n,e) = pub.getKey
    n+";"+e
  }
  def privateKeyToString(priv:SK) : String = {
    val (n,d) = priv.getKey
    n+";"+d
  }
  def publicKeyFromString(pub:String) : PK = {
    val keys=pub.split(";").map(x=> BigInt(x))
    new RSAPublicKey(keys(0),keys(1))
  }
  def privateKeyFromString(priv:String) : SK = {
    val keys=priv.split(";").map(x=> BigInt(x))
    new RSAPrivateKey(keys(0),keys(1))
  }

}

object RSA {
  var bytes=1024

  def setBytes(b : Int) : Unit = {
    RSA.bytes = b
  }
}
