import org.scalatest.{Matchers, FlatSpec}

class AlphabetCipherSpec extends FlatSpec with Matchers {
  behavior of "AlphabetCipherSpec"

  it should "return same character combined with A" in {
    assert(AlphabetCipher.encode('a','a') === 'a')
    assert(AlphabetCipher.encode('c','a') === 'c')
    assert(AlphabetCipher.encode('a','c') === 'c')
  }

  it should "return the encoded character when combined with Z" in {
    assert(AlphabetCipher.encode('z','f') === 'e')
    assert(AlphabetCipher.encode('z','a') === 'z')
    assert(AlphabetCipher.encode('z','z') === 'y')
  }

  it should "return the password with the rigth length" in {
    assert(AlphabetCipher.cypherKey("scones", 3) === "sco")
    assert(AlphabetCipher.cypherKey("scones", 15) === "sconessconessco")
  }
}
