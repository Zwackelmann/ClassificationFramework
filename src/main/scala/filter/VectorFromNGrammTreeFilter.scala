package filter
import format.arff_json.ArffJsonInstance
import parser.ArffJsonInstancesSource
import java.util.TreeMap
import format.arff_json.SparseArffJsonInstance
import format.arff_json.ArffJsonHeader
import format.arff_json.NumericArffJsonAttribute
import java.io.File
import format.arff_json.ArffJsonInstance
import format.arff_json.DenseArffJsonInstance
import parser.ContentDescription
import common.Common.FileConversion._

object VectorFromNGramTreeFilter {
    import filter.VectorFromDictFilter.AdvancedTokenizer._
    
    def apply(confName: String, nGrams: Iterable[List[String]]): FilterFactory = {
        new FilterFactory with Loadable[VectorFromNGramTreeFilter] {
            def apply(trainBase: ArffJsonInstancesSource) = {
                val filter = confName match {
                    case "conf1" => new Conf1(nGrams)
                    case _ => throw new RuntimeException("Unknown VectorFromNGrammTreeFilter configuration: " + confName)
                }
                filter
            }
            
            val historyAppendix = "vec-ng-" + confName
        }
    }
    
    def apply(confName: String, nGrammFile: File): FilterFactory = apply(confName, nGrammFile.lines.map(line => line.split("\\s+").toList))
    
    @serializable
    class Conf1(nGrams: Iterable[List[String]]) extends VectorFromNGramTreeFilter(nGrams) {
        val wordTransformFunction = (word: String) => stemmer.stem(word
            .filter(c => c.isDigit || c.isLetter)
            .toLowerCase()
        )
        
        override def inst2Words(inst: ArffJsonInstance) = {
            val text = inst.data(0).asInstanceOf[String]
            val words = tokenize(text)
                .filter(t => t.isInstanceOf[TokenString])
                .map(t => wordTransformFunction(t.asInstanceOf[TokenString].str))
            
            words
        }
    }
}

@serializable
abstract class VectorFromNGramTreeFilter(nGrams: Iterable[List[String]]) extends GlobalFilter {
    @transient lazy val stemmer = new PorterStemmer()
    val wordTransformFunction: (String => String)
    
    def inst2Words(inst: ArffJsonInstance): Seq[String]
    
    lazy val dict: NGramTree = {
        val buffer = new NGramTreeBuffer
        
        for(nGram <- nGrams) {
            buffer += nGram.map(part => wordTransformFunction(part))
        }
        
        buffer.toNGrammTree
    }
    
    def applyFilter(source: ArffJsonInstancesSource) = {
        source.map(
            elemFun = ((inst: ArffJsonInstance) => {
                def fun(words: Seq[String], ids: List[Int]): List[Int] = {
                    if(words.isEmpty) ids
                    else fun(words.tail, ((dict(words) match { case Some(id) => List(id) case None => List()}) ::: ids))
                }
                val ids = fun(inst2Words(inst), List())
                val data = ids.groupBy(a => a).map(a => a._1 -> a._2.size.toDouble)
                new SparseArffJsonInstance(inst.id, inst.categories, data.toMap, dict.nGramSet.size)
            }),
            headerFun = header => new ArffJsonHeader(
                header.relationName, 
                dict.sortedNGramList.map(w => new NumericArffJsonAttribute(w.mkString("+"))).toList
            )
        )
    }
}

















