package motylwg.crypto.test

import motylwg.crypto.CryptoString

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CryptoTest extends FunSuite {

  test("to and from ascii is consistent") {
    val s = "The quick brown fox"
    val cs = CryptoString(s)
    assert(cs.toString === s)
  }

  test("toHex inverts fromHex") {
    val s = "414243abcd"
    val cs = CryptoString.fromHex(s)
    assert(cs.toHex() === s)
  }

  test("fromHex") {
    val s = CryptoString.fromHex("616263")
    assert(s.toString === "abc")
  }


}