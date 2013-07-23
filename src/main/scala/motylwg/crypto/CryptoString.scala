package motylwg.crypto

import java.math.BigInteger
import scala.BigInt
import java.security.MessageDigest

case class CryptoString(bytes: List[Byte]) {

  lazy val buf = bytes.toArray
  lazy val bigint = new BigInteger(buf)

  def size = bytes.size

  def toBytes = bytes


  def inc = {
    new CryptoString(bigint.add(BigInteger.ONE).toByteArray.toList)
  }

  def ^ (that: CryptoString) = {
    val pairs = bytes.zip(that.toBytes)
    val xored = pairs.map {p => (p._1 ^ p._2).toByte}

    new CryptoString(xored)
  }

  def ++ (that: CryptoString) = {
    CryptoString(this.toString() + that.toString())
  }

  def sha256() = {
    new CryptoString(CryptoString.sha256.digest(buf).toList)
  }

  def toBlocks(blockSize: Int = 16): List[CryptoString] = {
    val s = this.pad(blockSize).toString
    val blocks = s.grouped(2 * blockSize).toList
    blocks.map (CryptoString(_))
  }

  def pad(blockSize: Int = 16) = {
    val npad = blockSize - bytes.size % blockSize
    val padded  =
      if (npad == 0)  //still need to pad if exact
        bytes.padTo(bytes.size + blockSize, blockSize.toByte)
      else
        bytes ++ (1 to npad).map {_ => npad.toByte}

    new CryptoString(padded)
  }

  private def byteToHex(b: Byte) : String = {
    val s = (b & 0xff).formatted("%h")
    if (s.length == 1) "0" + s else s
  }

  override def toString = {
    val strings = bytes.map {b => byteToHex(b)}
    strings.mkString
  }

  def toAscii = {
    val chars = bytes.map {b => b.toChar}
    chars.mkString
  }
}

object CryptoString {

  def apply(s: String)  = {
    require(s.length % 2 == 0)

    val bytes = for {
      i <- 0 until s.length / 2
      byte = s.substring(2 * i , 2 * i + 2)
    } yield Integer.valueOf(byte, 16).toByte

    new CryptoString(bytes.toList)

  }

  def fromAscii(s: String) = {
    val bytes = for (c <- s) yield c.toByte

    new CryptoString(bytes.toList)
  }

  def sha256 = MessageDigest.getInstance("SHA-256")
}