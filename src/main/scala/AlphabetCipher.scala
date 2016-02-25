object AlphabetCipher {
  def decipher(pass: String, codedMessage: String) = {
    cypherKey(pass,codedMessage.length).zip(codedMessage).map((decode _).tupled).mkString
  }


  def decode(row: Char, column: Char) = {
    val numLetters = 'z' - 'a' + 1
    val desp = (column - row + numLetters) % numLetters
    ('a' + desp).toChar
  }

  def cipher(s: String, s1: String): String = {
    cypherKey(s,s1.length).zip(s1).map((encode _).tupled).mkString
  }

  def cypherKey(pass: String, length: Int) = {
    (pass * (length / pass.length + 1)).take(length)
  }

  def encode(row: Char, column: Char): Char = {
    val rowDesp = row - 'a'
    val columnDesp = column - 'a'
    val numLetters = 'z' - 'a' + 1
    val totalDesp = (rowDesp + columnDesp) % numLetters
    ('a' + totalDesp).toChar
  }

}
