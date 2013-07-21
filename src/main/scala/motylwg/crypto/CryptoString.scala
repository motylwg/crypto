package motylwg.crypto

import java.math.BigInteger
import scala.BigInt
import java.security.MessageDigest

case class CryptoString(val bytes: Array[Byte]) {

  def toBytes() = bytes

  def length() = bytes.length

  lazy val bigint = new BigInteger(bytes)

  def inc = {
    new CryptoString((bigint.add(BigInteger.ONE)).toByteArray)
  }

  def ^ (that: CryptoString) = {
    val pairs = bytes.zip(that.toBytes())
    val xored = pairs.map {p => (p._1 ^ p._2).toByte}

    new CryptoString(xored.toList.toArray)
  }

  def + (that: CryptoString) = {
    val pairs = bytes.zip(that.toBytes())
    val xored = pairs.map {p => (p._1 ^ p._2).toByte}

    CryptoString.fromHex(this.toString() + that.toString())
  }

  def toBlocks(blockSize: Int = 16): Array[CryptoString] = {
    val n = (bytes.length - 1) / blockSize + 1
    val lastn = bytes.length % blockSize
    val hex = toString()

    val blocks = for (i <- 0 until n) yield
      if (i < n - 1) CryptoString.fromHex(hex.substring(2 * blockSize * i, 2 * blockSize * (i + 1)))
      else CryptoString.fromHex(hex.substring(2 * blockSize * i))

    blocks.toArray
  }

  def pad() = {
    val list = bytes.toList
    val npad = 16 - list.length % 16
    val padded = list ++ (1 to npad).map {_ => npad.toByte}
    new CryptoString(padded.toArray)
  }

  private def byteToHex(b: Byte) : String = {
    val bb = (b.toInt + 256) % 256
    val s = bb.formatted("%h")
    if (s.length == 1) "0" + s else s
  }

  def toHex() = {
    val strings = bytes.map {b => byteToHex(b)}
    strings.mkString
  }

  def sha256() = {
    new CryptoString(CryptoString.md.digest(bytes))
  }

  override def toString() = {
    val chars = bytes.map {b => b.toChar}
    chars.mkString
  }
}

object CryptoString {

  def apply(s: String)  = {
    val bytes = for (c <- s) yield c.toByte

    new CryptoString(bytes.toArray)
  }

  def fromHex(s: String) = {
    require(s.length % 2 == 0)

    val bytes = for {
      i <- 0 until s.length / 2
      byte = s.substring(2 * i , 2 * i + 2)
    } yield Integer.valueOf(byte, 16).toByte

    new CryptoString(bytes.toArray)
  }

  def md = MessageDigest.getInstance("SHA-256")
}