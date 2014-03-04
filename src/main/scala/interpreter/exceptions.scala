/**
* exceptions.scala - Definitions of exceptions
* @author Lanvin Victor Thiré François
* Copyright (c) 2014 GPLv3. See LICENCE file
*/

case class noIntegerPublicKey(s: String) extends Exception(s)

case class noIntegerSecretKey(s: String) extends Exception(s)

case class parsingError(s: String) extends Exception(s)

case class interpretationError(s: String) extends Exception(s)