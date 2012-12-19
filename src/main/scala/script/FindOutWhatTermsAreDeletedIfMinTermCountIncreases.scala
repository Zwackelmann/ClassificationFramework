package script
import filter.VectorFromDictFilter
import filter.VectorFromDictFilter.AdvancedTokenizer
import format.arff_json.ArffJsonInstance
import filter.Dictionary
import java.io.File
import parser.ContentDescription
import scala.collection.JavaConversions._
import parser.ArffJsonInstancesSource

object FindOutWhatTermsAreDeletedIfMinTermCountIncreases {
    def main(args: Array[String]) {
        val source = ArffJsonInstancesSource(new File("data/arffJson/exp-train_proj-abs.json"), ContentDescription("exp", ContentDescription.TrainSet, List()))
        
        val filters = (for(minCount <- List(3, 5, 10, 15, 25, 50, 100)) yield {
            val filter = new VectorFromDictFilter() {
                import AdvancedTokenizer._
                
                val wordTransformFunction = (word: String) => stemmer.stem(word
                    .filter(c => c.isDigit || c.isLetter)
                    .toLowerCase()
                )
                
                def inst2Words(inst: ArffJsonInstance) = {
                    val text = inst.data(0).asInstanceOf[String]
                    
                    val words = replaceAcronyms2(
                        tokenize(text, separators)
                            .filter(t => t.isInstanceOf[TokenString] || t.isInstanceOf[BracedText] || t.isInstanceOf[Formula] || t.isInstanceOf[Author])
                        ).flatMap(t => 
                            t match {
                                case TokenString(str) => if(stopList.contains(str)) List() else List(wordTransformFunction(str))
                                case Author(str) => List("{" + str.toLowerCase() + "}")
                                case Formula(str) => List("$" + str.filter(c => c.isDigit || c.isLetter).toLowerCase() + "$")
                                case _ => List()
                            }
                        )
                        .filter(s => s != "")
                    
                    words
                }
                
                val separators = List('.', ':', ';', ',')
                
                override val dict = new Dictionary() {
                    override val minWordCount = minCount
                }
            }
            
            filter.buildDict(source)
            
            filter.dict
        })
        
        val x = filters(0).dict -- filters(1).dict
        println(x.mkString("\n"))
        
    }
}

















