object AlphabetCipher {
  private val alphabet = 'a' to 'z'
  private val alphabetSize = alphabet.size

  private def transformMessage(transformCharWithKey: (Char,Char) => Char)
                              (pass: String, message: String): String = {
    key(pass).zip(message).map{
      case (key, char) => transformCharWithKey(key, char)
    }.mkString
  }
  
  def cipher(pass: String, decodedMessage: String): String = transformMessage(encodeCharWithKey)(pass, decodedMessage)
  def decipher(pass: String, codedMessage: String): String = transformMessage(decodeCharWithKey)(pass, codedMessage)

  def key(pass: String): Stream[Char] = Stream.continually(pass.toStream).flatten
  private def alphabetCharToInt(c: Char): Int = c - alphabet(0)
  private def intToAlphabetChar(n: Int): Char = (n + alphabet(0)).toChar
  private val encodeOp: (Int,Int) => Int = _ + _
  private val decodeOp: (Int,Int) => Int = _ - _
  private def transformCharWithKey(key: Char, char: Char)(op: (Int,Int) => Int): Char = intToAlphabetChar(Math.floorMod(op(alphabetCharToInt(char), alphabetCharToInt(key)),alphabetSize))
  def decodeCharWithKey(key:Char, char: Char): Char = transformCharWithKey(key,char)(decodeOp)
  def encodeCharWithKey(key:Char, char: Char): Char = transformCharWithKey(key,char)(encodeOp)

}

