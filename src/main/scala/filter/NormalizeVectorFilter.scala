package filter
import parser.ArffJsonInstancesSource
import format.arff_json.SparseArffJsonInstance
import java.io.File
import format.arff_json.HistoryItem

object NormalizeVectorFilter {
    def apply() = {
        new FilterFactory() {
            def apply(trainBase: ArffJsonInstancesSource) = new NormalizeVectorFilter(this)
            val historyAppendix = "norm"
        }
    }
}

class NormalizeVectorFilter(val historyAppendix: HistoryItem) extends GlobalFilter {
    def applyFilter(source: ArffJsonInstancesSource) = {
        println("use normalized filter " + source.contentDescription)
        
        source.map(
            elemFun = elements => {
                elements.map(inst => {
                    val len = (0.0 /: inst.sparseData.map(_._2).map(a => a*a))(_ + _)
                    val data = if(len != 0) {
                        inst.sparseData.map(kv => kv._1 -> kv._2.asInstanceOf[Double] / len)
                    } else {
                        Map[Int, Double]()
                    }
                    
                    new SparseArffJsonInstance(inst.id, inst.mscClasses, data, inst.numAttributes())
                })
            },
            headerFun = header => header,
            historyAppendix = historyAppendix
        )
    }
}