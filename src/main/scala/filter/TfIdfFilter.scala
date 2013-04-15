package filter
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonHeader
import scala.collection.mutable.HashMap
import java.io.File
import format.arff_json.SparseArffJsonInstance
import java.io.FileReader
import parser.ArffJsonInstancesSource
import classifier.CategoryIs
import parser.History

object TfIdfFilter {
    def apply() = new FilterFactory with Loadable[TfIdfFilter] {
        def apply(trainBase: ArffJsonInstancesSource) = new TfIdfFilter(trainBase)
        val historyAppendix = "tf-idf"
    }
    
    @serializable
    trait Appendix extends History {
        abstract override def apply(categoryIs: CategoryIs) = super.apply(categoryIs) :+ TfIdfFilter()
    }
}



@serializable
class TfIdfFilter(source: ArffJsonInstancesSource) extends GlobalFilter {
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
            elemFun = ((inst: ArffJsonInstance) => {
                val data = for((key, value) <- inst.sparseData) yield {
                    if(value == 0) key -> 0.0 
                    else {
                        key -> (value * math.log((numDocuments + 0.5) / (tf.getOrElse(key, 0) + 0.5)))
                    }
                }
                
                new SparseArffJsonInstance(inst.id, inst.categories, data, inst.numAttributes())
            }),
            headerFun = header => header
        )
    }
}






















