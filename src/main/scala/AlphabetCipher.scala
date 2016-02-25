object AlphabetCipher {
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
