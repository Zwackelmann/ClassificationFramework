package script
import filter.VectorFromDictFilter
import filter.VectorFromDictFilter.AdvancedTokenizer
import format.arff_json.ArffJsonInstance
import filter.Dictionary
import format.arff_json.HistoryItem
import java.io.File
import parser.ArffJsonInstancesFile2
import parser.ContentDescription
import scala.collection.JavaConversions._

object FindOutWhatTermsAreDeletedIfMinTermCountIncreases {
    def main(args: Array[String]) {
        val source = new ArffJsonInstancesFile2(new File("data/arffJson/exp-train_proj-abs.json"), ContentDescription("exp", ContentDescription.TrainSet, List()))
        
        val erg = (for(minCount <- List(15, 25)) yield {
            println("start: " + minCount)
            val filter = new VectorFromDictFilter {
                import AdvancedTokenizer._
                
                val historyAppendix = HistoryItem("")
                
                def inst2Words(inst: ArffJsonInstance) = {
                    val text = inst.data(0).asInstanceOf[String]
                    
                    transformStringTokens(
                        replaceAcronyms(
                            TokenGroup.group(
                                splitMultipleAuthors(
                                    tokenize(text).filter(t => true)
                                )
                            )
                        )
                    ).map(_.toString)
                }
                
                override val dict = new Dictionary() {
                    override val minWordCount = minCount
                }
            }
            
            filter.buildDict(source)
            
            (minCount, filter.dict)
        })
        
        
        val x = erg(0)._2.dict -- erg(1)._2.dict
        println(x.mkString("\n"))
    }
}

















