import org.scalatest.{Matchers, FlatSpec}

class AlphabetCipherSpec extends FlatSpec with Matchers {
  behavior of "AlphabetCipherSpec"

  it should "return the password with the rigth length" in {
    assert(AlphabetCipher.key("scones").take(3).mkString === "sco")
    assert(AlphabetCipher.key("scones").take(15).mkString === "sconessconessco")
  }

  it should "encode the message" in {
    assert(AlphabetCipher.cipher("scones", "meetmebythetree") === "egsgqwtahuiljgs")
    assert(AlphabetCipher.cipher("abcd", "cryptoisshortforcryptography") === "csastpkvsiqutgqucsastpiuaqjb")
  }

  it should "decode the message" in {
    assert(AlphabetCipher.decipher("scones", "egsgqwtahuiljgs") === "meetmebythetree")
    assert(AlphabetCipher.decipher("abcd", "csastpkvsiqutgqucsastpiuaqjb") === "cryptoisshortforcryptography")
  }

}
