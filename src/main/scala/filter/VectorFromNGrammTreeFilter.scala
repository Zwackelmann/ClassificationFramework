package filter
import format.arff_json.ArffJsonInstance
import parser.ArffJsonInstancesSource
import java.util.TreeMap
import format.arff_json.SparseArffJsonInstance
import format.arff_json.ArffJsonHeader
import format.arff_json.NumericArffJsonAttribute
import format.arff_json.HistoryItem
import java.io.File
import format.arff_json.ArffJsonInstance
import format.arff_json.DenseArffJsonInstance
import parser.ContentDescription
import common.Common.FileConversion._

object VectorFromNGramTreeFilter {
    def apply(confName: String, nGrams: Iterable[List[String]]): FilterFactory = {
        new StorableFilterFactory {
            def apply(trainBase: ArffJsonInstancesSource) = {
                val filter = confName match {
                    case "conf1" => new Conf1(nGrams, this)
                    case _ => throw new RuntimeException("Unknown VectorFromNGrammTreeFilter configuration: " + confName)
                }
                filter
            }
            
            val historyAppendix = "vec-ng-" + confName
            def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[VectorFromNGramTreeFilter]
        }
    }
    
    def apply(confName: String, nGrammFile: File): FilterFactory = apply(confName, nGrammFile.lines.map(line => line.split("\\s+").toList))
    
    @serializable
    class Conf1(nGrams: Iterable[List[String]], historyAppendix: HistoryItem) extends VectorFromNGramTreeFilter(nGrams, historyAppendix) {
        override def inst2Words(inst: ArffJsonInstance) = {
            inst.data(0)
                .asInstanceOf[String]
                .toLowerCase()
                .split("[\\s+\\.,]")
                .map(s => s.filter(c => c == '-' || c.isLetter || c.isDigit || c.isWhitespace))
                .map(s => if(s == "has") "ha" else s)
                .filter(_ != "")
                .toSeq
        }
    }
}

@serializable
abstract class VectorFromNGramTreeFilter(nGrams: Iterable[List[String]], val historyAppendix: HistoryItem) extends GlobalFilter {
    def inst2Words(inst: ArffJsonInstance): Seq[String]
    
    val dict: NGramTree = {
        val buffer = new NGramTreeBuffer
        
        for(nGram <- nGrams) {
            buffer += nGram
        }
        
        buffer.toNGrammTree
    }
    
    def applyFilter(source: ArffJsonInstancesSource) = {
        source.map(
            elemFun = elements => elements.map(inst => {
                def fun(words: Seq[String], ids: List[Int]): List[Int] = {
                    if(words.isEmpty) ids
                    else fun(words.tail, ((dict(words) match { case Some(id) => List(id) case None => List()}) ::: ids))
                }
                val ids = fun(inst2Words(inst), List())
                val data = ids.groupBy(a => a).map(a => a._1 -> a._2.size.toDouble)
                new SparseArffJsonInstance(inst.id, inst.mscClasses, data.toMap, dict.nGramSet.size)
            }),
            headerFun = header => new ArffJsonHeader(
                header.relationName, 
                dict.sortedNGramList.map(w => new NumericArffJsonAttribute(w.mkString("+"))).toList, List()
            ),
            historyAppendix = historyAppendix
        )
    }
}