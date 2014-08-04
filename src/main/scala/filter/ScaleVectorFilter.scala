package filter
import parser.ArffJsonInstancesSource
import java.io.File
import format.arff_json.ArffJsonInstance
import parser.History
import classifier.CategoryIs
import format.arff_json.ArffJsonHeader

object ScaleVectorFilter {
    def apply(scaleFactor: Double) = {
        new FilterFactory() {
            def apply(trainBase: ArffJsonInstancesSource) = new ScaleVectorFilter(scaleFactor)
            val historyAppendix = "scale-" + scaleFactor
        }
    }
    
    trait Appendix extends History with Serializable {
        val scaleFactor: Double
        abstract override def apply(categoryIs: CategoryIs) = super.apply(categoryIs) :+ ScaleVectorFilter(scaleFactor)
    }
}

class ScaleVectorFilter(val scaleFactor: Double) extends GlobalFilter {
    def applyFilter(source: ArffJsonInstancesSource) = {
        source.map(
            elemFun = (inst: ArffJsonInstance) => {
                ArffJsonInstance(
                    inst.id, 
                    inst.categories, 
                    inst.sparseData.mapValues(_ * scaleFactor), 
                    inst.numAttributes()
                )
            },
            headerFun = header => header
        )
    }
    
    override val trainingParams = None
}








