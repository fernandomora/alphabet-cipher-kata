object AlphabetCipher {
  private val alphabet: IndexedSeq[Char] = 'a' to 'z'

  def cipher(pass: String, decodedMessage: String): String = vigenèreCipher(vigenèreTableTranslation(_ + _))(pass, decodedMessage)

  def decipher(pass: String, codedMessage: String): String = vigenèreCipher(vigenèreTableTranslation(_ - _))(pass, codedMessage)

  private def vigenèreCipher(tableTranslation: (Char, Char) => Char)(pass: String, message: String): String =
    Stream.continually(pass).flatten.zip(message).map(tableTranslation.tupled).mkString

  private def vigenèreTableTranslation(op: (Int, Int) => Int)(key: Char, char: Char): Char =
    alphabet(Math.floorMod(op(alphabet.indexOf(char), alphabet.indexOf(key)), alphabet.size))
}

