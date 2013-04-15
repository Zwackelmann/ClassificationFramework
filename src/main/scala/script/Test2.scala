package script
import java.io.File
import parser.ContentDescription
import filter.VectorFromNGramTreeFilter
import common.Common.FileConversion._
import scala.collection.mutable.HashMap

object Test2 {
    import filter.VectorFromDictFilter.AdvancedTokenizer._
    
    val separators = List('.', ':', ';', ',')
    
    val wordTransformFunction = (word: String) => if(stopList.contains(word.toLowerCase())) "" else { 
        stemmer.stem(word
            .filter(c => c.isDigit || c.isLetter)
            .toLowerCase()
        )
    }
    
    def replaceAcronyms(tokenList: List[Token]) = {
            val acronyms = new HashMap[Token, List[Token]]
            
            for((g, i) <- tokenList.zipWithIndex) {
                g match {
                    case BracedText(text) if text.size > 1 && text.forall(c => c.isUpper) && i>text.size => 
                        val candidates = tokenList.slice(i-text.size, i)
                        if(
                            candidates.forall(c => c.isInstanceOf[TokenString]) && 
                            text.toUpperCase.zip(
                                candidates.map(c => 
                                    c.asInstanceOf[TokenString].str.charAt(0).toUpper
                                )
                            ).map(p => p._1 == p._2).reduceLeft(_ && _)) {
                            acronyms += (new TokenString(text) -> candidates)
                        }
                    case _ => 
                }
            }
            
            tokenList.flatMap(t => {
                if(acronyms.keySet.contains(t)) 
                    acronyms(t) 
                else if(t.isInstanceOf[BracedText]) {
                    List()
                } else {
                    List(t)
                }
            })
        }
    
    def main(args: Array[String]) {
        val text = "The graph isomorphism (GI) plays a $k$-blah central role in the theory of computational complexity and has importance in physics and chemistry as well {cite kobler93,fortin96}. No polynomial-time algorithm for solving GI is known. We investigate classical and quantum physics-based polynomial-time algorithms for solving the graph isomorphism problem in which the graph structure is reflected in the behavior of a dynamical system. We show that a classical dynamical algorithm proposed by Gudkov and Nussinov {cite gudkov02} as well as its simplest quantum generalization fail to distinguish pairs of non-isomorphic strongly regular graphs. However, by combining the algorithm of Gudkov and Nussinov with a construction proposed by Rudolph {cite rudolph02} in which one examines a graph describing the dynamics of two particles on the original graph, we find an algorithm that successfully distinguishes all pairs of non-isomorphic strongly regular graphs that we tested with up to 29 vertices."
        /*val words = tokenize(text, separators)
            .filter(t => t.isInstanceOf[TokenString])
            .map(t => wordTransformFunction(t.asInstanceOf[TokenString].str))
            .filter(t => t != "")
            
        println(words)*/
        
        /*val words = replaceAcronyms(
            tokenize(text, separators)
                .filter(t => t.isInstanceOf[TokenString] || t.isInstanceOf[BracedText] || t.isInstanceOf[Formula] || t.isInstanceOf[Author])
            ).flatMap(t => 
                t match {
                    case TokenString(str) => if(stopList.contains(str)) List() else List(wordTransformFunction(str))
                    case Author(str) => List("{" + str.toLowerCase() + "}")
                    case Formula(str) => List("$" + str.filter(c => c.isDigit || c.isLetter).toLowerCase() + "$")
                    case BracedText(str) => List("(" + str + ")")
                    case _ => List()
                }
            )
            .filter(s => s != "")
            
        println(words)*/
    }
}








