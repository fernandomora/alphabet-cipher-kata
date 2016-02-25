object AlphabetCipher {
  def encode(row: Char, column: Char): Char = {
    val rowDesp = row - 'a'
    val colDesp = column - 'a'
    ('a' + rowDesp + colDesp).toChar
  }

}
