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
import format.arff_json.HistoryItem

object TfIdfFilter {
    def apply() = new StorableFilterFactory {
        def apply(trainBase: ArffJsonInstancesSource) = new TfIdfFilter(trainBase, this)
        val historyAppendix = "tf-idf"
        def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[TfIdfFilter]
    }
}

@serializable
class TfIdfFilter(source: ArffJsonInstancesSource, val historyAppendix: HistoryItem) extends GlobalFilter {
    val (tf, numDocuments) = {
        val tf = new HashMap[Int, Int] {
            override def default(key: Int) = 0
        }
        
        var count = 0
        for(example <- source.iterator) {
            for((key, value) <- example.sparseData if value != 0) {
                tf(key) = tf(key) + 1
            }
            count = count + 1
        }
        
        (tf.toMap, count)
    }
    
    def applyFilter(source: ArffJsonInstancesSource) = {
        source.map(
            elemFun = elements => elements.map(inst => {
                val data = for((key, value) <- inst.sparseData) yield {
                    key -> (value * math.log((numDocuments + 0.5) / (tf.getOrElse(key, 0) + 0.5)))
                }
                
                new SparseArffJsonInstance(inst.id, inst.mscClasses, data, inst.numAttributes())
            }),
            headerFun = header => header,
            historyAppendix = historyAppendix
        )
    }
}






















