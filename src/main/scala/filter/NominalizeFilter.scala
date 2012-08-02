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
import parser.ArffJsonInstancesFile
import parser.ArffJsonInstancesSource

object NominalizeFilter {
    class Conf1(val historyAppendix: String) extends NominalizeFilter {
        override def inst2Words(inst: ArffJsonInstance) = inst.data(0).asInstanceOf[List[String]].toSeq
        override def wordFun(word: String) = word.filter(_.isLetter).toLowerCase()
        
        override def attributeName = "journal_index"
    }
    
    def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[NominalizeFilter]
}

abstract class NominalizeFilter extends GlobalFilter with Serializable {
    val dict: JavaSet[String] = new TreeSet[String]()
    
    def inst2Words(inst: ArffJsonInstance): Seq[String]
    def wordFun(word: String): String
    def attributeName = "nominalized"
    
    def orderedDict = dict.toList.sortBy(s => s)
        
    def expandDict(source: ArffJsonInstancesSource) {
        for(inst <- source.iterator) {
            for(word <- inst2Words(inst)) {
                dict.add(wordFun(word))
            }
        }
        dict.remove("")
    }
    
    override def applyFilter(source: ArffJsonInstancesSource) = {
        println("use normalize filter on " + source.contentDescription)
        
        val word2IndexMap = (for((word, i) <- (orderedDict.zipWithIndex)) yield word -> i).toMap
        
        source.map(
            elements => (for(inst <- elements) yield {
                val words = inst2Words(inst)
                
                new DenseArffJsonInstance(
                    inst.id,
                    inst.mscClasses,
                    words
                        .filter(w => dict.contains(wordFun(w)))
                        .map(w => word2IndexMap(wordFun(w)).toDouble)
                        .toList
                )
            }),
            headerMapping _,
            historyAppendix
        )
    }
    
    def headerMapping(header: ArffJsonHeader) = new ArffJsonHeader(
        header.relationName,
        List(new NominalArffJsonAttribute(attributeName, orderedDict)),
        List()
    )
}








