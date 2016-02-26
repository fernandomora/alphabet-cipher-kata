import scala.collection.immutable.NumericRange.Inclusive

object AlphabetCipher {
  private val alphabet: Inclusive[Char] = 'a' to 'z'

  private def transformMessage(transformCharWithKey: (Char, Char) => Char)(pass: String, message: String): String =
    key(pass).zip(message).map(transformCharWithKey.tupled).mkString

  def key(pass: String): Stream[Char] = Stream.continually(pass.toStream).flatten

  def cipher(pass: String, decodedMessage: String): String = transformMessage(vigenèreCipher(_ + _))(pass, decodedMessage)

  def decipher(pass: String, codedMessage: String): String = transformMessage(vigenèreCipher(_ - _))(pass, codedMessage)

  private def vigenèreCipher(op: (Int, Int) => Int)(key: Char, char: Char): Char = {
    alphabet(
      Math.floorMod(
        op(alphabet.indexOf(char), alphabet.indexOf(key)),
        alphabet.size)
    )
  }
}

