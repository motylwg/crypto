import motylwg.crypto._






































































































val s1 = CryptoString.fromHex("112234")
val s2 = CryptoString.fromHex("012112")

(s1 ^ s2).toHex
