package filter
import parser.ArffJsonInstancesSource
import java.io.File
import format.arff_json.ArffJsonInstance
import parser.History
import classifier.CategoryIs
import format.arff_json.ArffJsonHeader

object NormalizeVectorFilter {
    def apply() = {
        new FilterFactory() {
            def apply(trainBase: ArffJsonInstancesSource) = new NormalizeVectorFilter()
            val historyAppendix = "norm"
        }
    }
    
    trait Appendix extends History with Serializable {
        abstract override def apply(categoryIs: CategoryIs) = super.apply(categoryIs) :+ NormalizeVectorFilter()
    }
    
    def main(args: Array[String]) {
        val source = ArffJsonInstancesSource(List(
            ArffJsonInstance("1", List("a", "b"), List(1.0, 2.5, -2.4)),
            ArffJsonInstance("2", List("a", "c"), List(3.45, 0.0, 0.1)),
            ArffJsonInstance("3", List("b"), List(-1.4, -2.5, -100.0))
        ),
        ArffJsonHeader(3))
        
        println(new NormalizeVectorFilter().applyFilter(source).toList.mkString("\n"))
    }
}

class NormalizeVectorFilter() extends GlobalFilter {
    def norm(inst: ArffJsonInstance) = if(inst.sparseData.isEmpty) 0.0
        else math.sqrt(inst.sparseData.values.map(a => a*a).reduceLeft(_ + _))
    
    def applyFilter(source: ArffJsonInstancesSource) = {
        source.map(
            elemFun = (inst: ArffJsonInstance) => {
                val len = norm(inst)
                val data = if(len != 0) {
                    inst.sparseData.map(kv => kv._1 -> kv._2 / len)
                } else {
                    Map[Int, Double]()
                }
                
                ArffJsonInstance(inst.id, inst.categories, data, inst.numAttributes())
            },
            headerFun = header => header
        )
    }
    
    override val trainingParams = None
}








