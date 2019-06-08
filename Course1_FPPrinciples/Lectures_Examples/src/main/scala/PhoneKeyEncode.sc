// Map of phone digit key to characters
val mnem = Map(
  '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
)

// Sample words
val words = List("Java", "Lava", "Kotlin")

// Invert the mnem Map to map Char to Digit key
val charCode: Map[Char, Char] =
  for ((digit, str) <- mnem; letter <- str) yield letter -> digit

// Map a word to string of digits it can represent
def wordCode(word: String): String = word.toUpperCase map charCode

wordCode("Java")

// Map a string of digits to words that represented
// Missing number will be map to empty set
val wordsForNum: Map[String, Seq[String]] =
  words groupBy wordCode withDefaultValue Seq()

// All ways to encode a digit string as a list of words
def encode(digits: String): Set[List[String]] = {
  if(digits.isEmpty) Set(List())
  else {
    for {
      split <- 1 to digits.length
      word <- wordsForNum(digits take split)
      rest <- encode(digits drop split)
    } yield word :: rest
  }.toSet
}

encode("5282568546")

def translate(number: String): Set[String] =
  encode(number) map (_ mkString " ")

translate("5282568546")