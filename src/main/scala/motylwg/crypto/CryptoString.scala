package motylwg.crypto

import java.math.BigInteger
import scala.BigInt
import java.security.MessageDigest

case class CryptoString(val bytes: List[Byte]) {

  def toBytes() = bytes

  def length() = bytes.length

  def byteToHex(b: Byte) : String = {
    val bb = (b.toInt + 256) % 256
    val s = bb.formatted("%h")
    if (s.length == 1) "0" + s else s
  }

  def toHex() = {
    val strings = bytes.map {b => byteToHex(b)}
    strings.mkString
  }

  override def toString() = {
    val chars = bytes.map {b => b.toChar}
    chars.mkString
  }
}

object CryptoString {

  def apply(s: String)  = {
    val bytes = for (c <- s) yield c.toByte

    new CryptoString(bytes.toList)
  }

  def fromHex(s: String) = {
    require(s.length % 2 == 0)

    val bytes = for {
      i <- 0 until s.length / 2
      byte = s.substring(2 * i , 2 * i + 2)
    } yield Integer.valueOf(byte, 16).toByte

    new CryptoString(bytes.toList)
  }
}