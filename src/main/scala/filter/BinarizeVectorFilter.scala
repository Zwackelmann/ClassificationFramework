package filter

import parser.ArffJsonInstancesSource
import parser.History
import classifier.CategoryIs
import format.arff_json.ArffJsonInstance

object BinarizeVectorFilter {
    def apply() = {
        new FilterFactory() {
            def apply(trainBase: ArffJsonInstancesSource) = new BinarizeVectorFilter()
            val historyAppendix = "binarized"
        }
    }
    
    @serializable
    trait Appendix extends History {
        abstract override def apply(categoryIs: CategoryIs) = super.apply(categoryIs) :+ BinarizeVectorFilter()
    }
}

class BinarizeVectorFilter() extends GlobalFilter {
    def applyFilter(source: ArffJsonInstancesSource) = {
        source.map(
            elemFun = (inst: ArffJsonInstance) => {
                ArffJsonInstance(
                    inst.id, 
                    inst.categories, 
                    inst.sparseData
                        .filter(kv => kv._2 > 0.001)
                        .map(kv => kv._1 -> 1.0), 
                    inst.numAttributes()
                )
            },
            headerFun = header => header
        )
    }
    
    override val trainingParams = None
}