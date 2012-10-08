package script
import filter.PorterStemmer

object Test {
    import filter.VectorFromDictFilter.AdvancedTokenizer._
    
    def main(args: Array[String]) {
        println(foobar("aladsf ealkfjda ealkfjsd asdfkljda-afldksj $$test$$-fasdfasdf [adfeek $akdkdkdk$[dkkdkddk]"))
    }
    
    val wordTransformFunction = (word: String) => stemmer.stem(word
        .filter(c => c.isDigit || c.isLetter)
        .toLowerCase()
    )
    
    def foobar(text: String) = {
        val words = tokenize(text, separators)
            .flatMap(t => 
                t match {
                    case TokenString(str) => if(stopList.contains(str)) List() else List(wordTransformFunction(str))
                    case Author(str) => List("{" + str.toLowerCase() + "}")
                    case Formula(str) => List("$" + str.filter(c => c.isDigit || c.isLetter).toLowerCase() + "$")
                    case _ => List()
                }
                
            )
            
        words
    }
    
     val separators = List('.', ':', ';', ',', '-')
}