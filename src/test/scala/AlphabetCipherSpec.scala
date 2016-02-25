import org.scalatest.{Matchers, FlatSpec}

class AlphabetCipherSpec extends FlatSpec with Matchers {
  behavior of "AlphabetCipherSpec"

  it should "should return same character combined with A" in {
    assert(AlphabetCipher.encode('a','a') === 'a')
    assert(AlphabetCipher.encode('c','a') === 'c')
    assert(AlphabetCipher.encode('a','c') === 'c')
  }



}
