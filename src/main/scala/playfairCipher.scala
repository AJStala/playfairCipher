import scala.io.Source
import java.io.File
import java.io.PrintWriter
import com.sun.tools.javac.util.Pair
import scala.collection.mutable.Map


object playfairCipher extends App{

  // Gets path for input text file
  val inputPath = getClass.getResource( "input.txt")

  //  reads data from text file using scala.io.Source
  val inputFile = Source.fromFile( inputPath.getFile, "UTF-8")

  /* converts the data from the text file to a list
     filters any characters that are not letters
     and converts all letters to upper case */
  val plainText = inputFile.toList.filter( { _.isLetter}).map( {_.toUpper } )

  println(plainText)



  //checks if the input text is even and if not adds a bogus character X to the end of the list
  def evenText(inputText: List[Any]) =
    if( inputText.length % 2 != 0) { inputText :+ "X" }
      else inputText

  //groups the text in pairs of two using .grouped
  def groupedText(inputText:List[Any]) = inputText.grouped(2).toList

  val text = groupedText(evenText(plainText))


  val cipherKeyWord = "eff.ks6ool4p"

  //takes a string and creats a virtual 5x5 array containing 25 of 26 letters of the alphabet
  def createKey(keyWord: String) =
    /*takes input string converts it to a list removes any characters that are not letters,
     converts all letters to uppercase, removes all repeat letters
     uses .toList, .filter, .map and .distinct */
    def cipherKeyWordSplit(keyWord: String) = keyWord.toList.filter( { _.isLetter}).map( {_.toUpper } ).distinct

    /*takes the input text and checks to see if it contains the letter J
      if the text does then it creates a list of letters excluding the letter I
      if the text does not then it creats a list of letters excluding the letter J */
    def keyLetters(plainText: List[List[Any]]) =
      if (plainText.contains('J')) {
        for(letter <- 'A' to 'Z' if letter != 'I')
          yield letter
      }
      else for(letter <- 'A' to 'Z' if letter != 'J')
          yield letter

    /* creats a key alphabet by combining keyword list and the letters list
       then removes any repeat letters using .distinct */
    val key = (cipherKeyWordSplit(keyWord) ++ keyLetters(text)).distinct

    val indexToCoord = (index: Int) => new Pair(index / 5, index % 5)

    val coordToIndex = (coordinates:Pair[Int, Int]) => coordinates.fst * 5 + coordinates.snd

    val alphabet = Map[Char, Pair[Int,Int]]()
      for( n <- 0 until key.size)
        alphabet(key(n)) = indexToCoord(n)

    val letterGetter = (coordinates:Pair[Int,Int]) =>
      key(coordToIndex(coordinates))

    val getCoord= (letter:Char) => alphabet(letter)

    for( key <- alphabet.keys) {
      val coordinates = alphabet(key)
      val row = coordinates.fst
      val col = coordinates.snd
      println(f"$key is at ($row, $col)")
   }

  val cipherTable = createKey(cipherKeyWord)

  // Writes a string to an output text file using java.io.File and java.io.PrintWriter
  /*
  val writer = PrintWriter( File("src/main/scala/output.txt"))
  writer.write("Test")
  writer.close()
  */
}
