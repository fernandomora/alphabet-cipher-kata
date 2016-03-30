import org.scalatest.{Matchers, FlatSpec}

class AlphabetCipherSpec extends FlatSpec with Matchers {
  behavior of "AlphabetCipherSpec"

  it should "encode the message" in {
    assert(AlphabetCipher.cipher("scones", "meetmebythetree") === "egsgqwtahuiljgs")
    assert(AlphabetCipher.cipher("abcd", "cryptoisshortforcryptography") === "csastpkvsiqutgqucsastpiuaqjb")
  }

  it should "decode the message" in {
    assert(AlphabetCipher.decipher("scones", "egsgqwtahuiljgs") === "meetmebythetree")
    assert(AlphabetCipher.decipher("abcd", "csastpkvsiqutgqucsastpiuaqjb") === "cryptoisshortforcryptography")
  }

}
