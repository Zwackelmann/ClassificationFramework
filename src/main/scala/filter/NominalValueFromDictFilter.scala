package filter

import java.io.File
import common.Common._
import format.arff_json.ArffJsonInstance
import java.util.TreeSet
import java.util.TreeMap
import java.util.{Set => JavaSet}
import java.io.BufferedWriter
import java.io.FileWriter
import scala.collection.mutable
import scala.collection.JavaConversions._
import format.arff_json.SparseArffJsonInstance
import format.arff_json.ArffJsonHeader
import format.arff_json.NumericArffJsonAttribute
import format.arff_json.DenseArffJsonInstance
import format.arff_json.NominalArffJsonAttribute
import parser.ArffJsonInstancesSource

object NominalValueFromDictFilter {
    def apply(confName: String): FilterFactory = {
        confName match {
            case "conf1" => new StorableFilterFactory() {
                def apply(trainBase: ArffJsonInstancesSource) = {
                    val filter = new Conf1
                    filter.expandDict(trainBase)
                    filter
                }
                
                val historyAppendix = "nominal-value-from-dict-" + confName
                
                def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[NominalValueFromDictFilter]
            }
            case _ => throw new RuntimeException("Unknown NominalValueFromDictFilter configuaration name: " + confName)
        }
    }
    
    class Conf1 extends NominalValueFromDictFilter {
        override def inst2Words(inst: ArffJsonInstance) = inst.data(0).asInstanceOf[List[String]].toSeq
        override def wordFun(word: String) = word.filter(_.isLetter).toLowerCase()
        
        override def attributeName = "journal_index"
    }
}

abstract class NominalValueFromDictFilter extends GlobalFilter with Serializable {
    val dict: JavaSet[String] = new TreeSet[String]()
    
    def inst2Words(inst: ArffJsonInstance): Seq[String]
    def wordFun(word: String): String
    def attributeName = "nominal_attribute"
    
    def expandDict(source: ArffJsonInstancesSource) {
        for(inst <- source.iterator) {
            for(word <- inst2Words(inst)) {
                dict.add(wordFun(word))
            }
        }
        dict.remove("")
    }
    
    override def applyFilter(source: ArffJsonInstancesSource) = {
        val word2IndexMap = (for((word, i) <- (dict.zipWithIndex)) yield word -> i).toMap
        
        source.map((inst: ArffJsonInstance) => {
            val words = inst2Words(inst)
            
            new SparseArffJsonInstance(
                inst.id,
                inst.categories,
                words
                    .filter(w => dict.contains(wordFun(w)))
                    .map(w => word2IndexMap(wordFun(w)) -> 1.0)
                    .toMap,
                source.header.attributes.size
            )},
            headerMapping _
        )
    }
    
    def headerMapping(header: ArffJsonHeader) = new ArffJsonHeader(
            header.relationName, 
            dict.toList.sortBy(s => s).map(word => new NumericArffJsonAttribute(word)).toList
        )
}








