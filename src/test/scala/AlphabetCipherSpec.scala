import org.scalatest.{Matchers, FlatSpec}

class AlphabetCipherSpec extends FlatSpec with Matchers {
  behavior of "AlphabetCipherSpec"

  it should "return same character combined with A" in {
    assert(AlphabetCipher.encodeCharWithKey('a','a') === 'a')
    assert(AlphabetCipher.encodeCharWithKey('c','a') === 'c')
    assert(AlphabetCipher.encodeCharWithKey('a','c') === 'c')
  }

  it should "return the encoded character when combined with Z" in {
    assert(AlphabetCipher.encodeCharWithKey('z','f') === 'e')
    assert(AlphabetCipher.encodeCharWithKey('z','a') === 'z')
    assert(AlphabetCipher.encodeCharWithKey('z','z') === 'y')
  }

  it should "return the password with the rigth length" in {
    assert(AlphabetCipher.key("scones").take(3).mkString === "sco")
    assert(AlphabetCipher.key("scones").take(15).mkString === "sconessconessco")
  }

  it should "encode the message" in {
    assert(AlphabetCipher.cipher("scones", "meetmebythetree") === "egsgqwtahuiljgs")
  }

  it should "decode a given character when combined with Z" in {
    assert(AlphabetCipher.decodeCharWithKey('z', 'e') === 'f')
    assert(AlphabetCipher.decodeCharWithKey('a', 'c') === 'c')
    assert(AlphabetCipher.decodeCharWithKey('z', 'y') === 'z')
    assert(AlphabetCipher.decodeCharWithKey('s', 'e') === 'm')
  }

  it should "decode the message" in {
    assert(AlphabetCipher.decipher("scones", "egsgqwtahuiljgs") === "meetmebythetree")
  }

}
