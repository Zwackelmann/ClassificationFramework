package filter
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonHeader
import scala.collection.mutable.HashMap
import java.io.File
import common.Time
import format.arff_json.SparseArffJsonInstance
import java.io.FileReader
import parser.ArffJsonInstancesFile
import parser.ArffJsonInstancesSource
import parser.ArffJsonInstancesSource

object TfIdfFilter {
    def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[TfIdfFilter]
}

@serializable
class TfIdfFilter(source: ArffJsonInstancesSource, val historyAppendix: String) extends GlobalFilter {
    println("train tf-idf filter with " + source.contentDescription)
    
    val (df, numDocuments) = {
        val df = new HashMap[Int, Int] {
            override def default(key: Int) = 0
        }
        
        var count = 0
        for(example <- source.iterator) {
            for((key, value) <- example.sparseData if value != 0) {
                df(key) = df(key) + 1
            }
            count = count + 1
        }
        
        (df.toMap, count)
    }
    
    def applyFilter(source: ArffJsonInstancesSource) = {
        println("use tf-idf filter on " + source.contentDescription)
        
        source.map(
            elemFun = elements => elements.map(inst => {
                val data = for((key, value) <- inst.sparseData) yield {
                    val v = value.asInstanceOf[Double]
                    key -> (v * math.log((numDocuments + 0.5) / (df.getOrElse(key, 0) + 0.5)))
                }
                
                new SparseArffJsonInstance(inst.id, inst.mscClasses, data, inst.numAttributes())
            }),
            headerFun = header => header,
            historyAppendix = historyAppendix
        )
    }
}






















