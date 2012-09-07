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

object VectorFromNGrammTreeFilter {
    def main(args: Array[String]) {
        val source = ArffJsonInstancesSource(
            List(
                new DenseArffJsonInstance("", List(), List("my fancy shit has all regulars locals rings and is analytic irreducible locals domain")),
                new DenseArffJsonInstance("", List(), List("you knew that all Regulars lOcals rings are all regulaR local ringS and not irreducible"))
            ),
            new ArffJsonHeader("", List(), List()),
            ContentDescription("", ContentDescription.TrainSet, List())
        )
        
        val filter = VectorFromNGrammTreeFilter("conf1", List(
            List("all", "regular", "local", "ring"),
            List("analytic", "irreducible", "local", "domain")
        ))(source)
        
        println(source.applyFilter(filter).mkString("\n"))
    }
    
    def apply(confName: String, nGramms: List[List[String]]) = {
        new StorableFilterFactory {
            def apply(trainBase: ArffJsonInstancesSource) = {
                val filter = confName match {
                    case "conf1" => new Conf1(nGramms, this)
                    case _ => throw new RuntimeException("Unknown VectorFromNGrammTreeFilter configuration: " + confName)
                }
                filter
            }
            
            val historyAppendix = "vec-ng-" + confName
            def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[VectorFromNGrammTreeFilter]
        }
    }
    
    class Conf1(nGrams: List[List[String]], historyAppendix: HistoryItem) extends VectorFromNGrammTreeFilter(nGrams, historyAppendix) {
        override def inst2Words(inst: ArffJsonInstance) = inst.data(0)
            .asInstanceOf[String]
            .toLowerCase()
            .filter(c => c == '-' || c.isLetter || c.isDigit || c.isWhitespace)
            .split("\\s+")
            .map(s => if(s.endsWith("s")) s.substring(0, s.size - 1) else s)
            .toSeq
    }
}

abstract class VectorFromNGrammTreeFilter(nGrams: List[List[String]], val historyAppendix: HistoryItem) extends GlobalFilter {
    def inst2Words(inst: ArffJsonInstance): Seq[String]
    
    val dict: NGrammTree = {
        val buffer = new NGrammTreeBuffer
        
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
                new SparseArffJsonInstance(inst.id, inst.mscClasses, data.toMap, dict.nGrammSet.size)
            }),
            headerFun = header => new ArffJsonHeader(
                header.relationName, 
                dict.sortedNGrammList.map(w => new NumericArffJsonAttribute(w.mkString("+"))).toList, List()
            ),
            historyAppendix = historyAppendix
        )
    }
}