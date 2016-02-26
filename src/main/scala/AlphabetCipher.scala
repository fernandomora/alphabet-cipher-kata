object AlphabetCipher {
  private val alphabet = 'a' to 'z'

  private def transformMessage(transformCharWithKey: (Char, Char) => Char)(pass: String, message: String): String =
    key(pass).zip(message).map(transformCharWithKey.tupled).mkString

  def key(pass: String): Stream[Char] = Stream.continually(pass.toStream).flatten

  def cipher(pass: String, decodedMessage: String): String = transformMessage(vigenèreCipher(_ + _))(pass, decodedMessage)

  def decipher(pass: String, codedMessage: String): String = transformMessage(vigenèreCipher(_ - _))(pass, codedMessage)

  private def vigenèreCipher(op: (Int, Int) => Int)(key: Char, char: Char): Char = {
    def alphabetCharToInt(c: Char): Int = c - alphabet(0)
    def intToAlphabetChar(n: Int): Char = (n + alphabet(0)).toChar
    intToAlphabetChar(Math.floorMod(op(alphabetCharToInt(char), alphabetCharToInt(key)), alphabet.size))
  }
}

