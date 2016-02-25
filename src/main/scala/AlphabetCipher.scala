object AlphabetCipher {
  def decipher(pass: String, codedMessage: String) = {
    applyFunction(decode)(pass, codedMessage)
  }

  private def applyFunction(f: (Char, Char) => Char)(pass: String, message: String) = {
    cypherKey(pass,message.length).zip(message).map( f.tupled ).mkString
  }

  def decode(row: Char, column: Char) = {
    val numLetters = 'z' - 'a' + 1
    val desp = (column - row + numLetters) % numLetters
    ('a' + desp).toChar
  }

  val cipher: (String, String) => String = {
    applyFunction(encode)
  }

  def cypherKey(pass: String, length: Int) = {
    Stream.continually(pass.toStream).flatten.take(length).toList.mkString
    //(pass * (length / pass.length + 1)).take(length)
  }

  def encode(row: Char, column: Char): Char = {
    val rowDesp = row - 'a'
    val columnDesp = column - 'a'
    val numLetters = 'z' - 'a' + 1
    val totalDesp = (rowDesp + columnDesp) % numLetters
    ('a' + totalDesp).toChar
  }

}
