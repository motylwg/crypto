package motylwg.crypto.test

import motylwg.crypto.CryptoString

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CryptoTest extends FunSuite {

  test("to and from string is consisten") {
    val s = "414243abcd"
    val cs = CryptoString(s)
    assert(cs.toString === s)
  }

  test("to and from ascii is consistent") {
    val s = "The quick brown fox"
    val cs = CryptoString.fromAscii(s)
    assert(cs.toAscii === s)
  }

  test("hax to ascii conversion") {
    val s = CryptoString("616263")
    assert(s.toAscii === "abc")
  }

  test("inc") {
    val s = CryptoString.fromAscii("abc")
    val sInc = CryptoString.fromAscii("abd")
    assert(s.inc === sInc)
  }

  test("concatentate") {
    val s1 = CryptoString("A1B2")
    val s2 = CryptoString("C3D4")
    assert(s1 ++ s2 === CryptoString("A1B2C3D4"))
  }

  test("XOR") {
    val s1 = CryptoString("112234")
    val s2 = CryptoString("012112")
    assert((s1 ^ s2) === CryptoString("100326"))
  }

  test("padding") {
    val s = CryptoString("000102030405060708090A0B")
    assert(s.pad() === CryptoString("000102030405060708090A0B04040404"))
  }

  test("padding with whole extra block") {
    val s = CryptoString("000102030405060708090A0B0C0D0E0F")
    assert(s.pad() === CryptoString("000102030405060708090A0B0C0D0E0F10101010101010101010101010101010"))
  }

  test("toBlocks") {
    val s = CryptoString("aabbcc")
    assert( s.toBlocks(2) === List(CryptoString("aabb"), CryptoString("cc01")))
  }
}