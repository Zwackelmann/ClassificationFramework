package filter
import parser.ArffJsonInstancesSource
import format.arff_json.SparseArffJsonInstance
import java.io.File
import format.arff_json.ArffJsonInstance
import parser.History
import classifier.TargetClassDefinition

object NormalizeVectorFilter {
    def apply() = {
        new FilterFactory() {
            def apply(trainBase: ArffJsonInstancesSource) = new NormalizeVectorFilter()
            val historyAppendix = "norm"
        }
    }
    
    @serializable
    trait Appendix extends History {
        abstract override def apply(targetClassDef: TargetClassDefinition) = super.apply(targetClassDef) :+ NormalizeVectorFilter()
    }
}

class NormalizeVectorFilter() extends GlobalFilter {
    def applyFilter(source: ArffJsonInstancesSource) = {
        
        source.map(
            elemFun = (inst: ArffJsonInstance) => {
                val len = math.sqrt((0.0 /: inst.sparseData.map(_._2).map(a => a*a))(_ + _))
                val data = if(len != 0) {
                    inst.sparseData.map(kv => kv._1 -> kv._2.asInstanceOf[Double] / len)
                } else {
                    Map[Int, Double]()
                }
                
                new SparseArffJsonInstance(inst.id, inst.categories, data, inst.numAttributes())
            },
            headerFun = header => header
        )
    }
}