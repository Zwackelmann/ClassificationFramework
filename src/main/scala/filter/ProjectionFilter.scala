package filter
import parser.ArffJsonInstancesSource
import format.arff_json.ArffJsonHeader
import format.arff_json.ArffJsonInstance
import parser.History
import classifier.CategoryIs

object ProjectionFilter {
    def apply(ids: List[Int], projectionDesc: String) = new FilterFactory {
        val _ids = ids
        val historyAppendix = "proj-" + projectionDesc
        
        @transient lazy val f = new ProjectionFilter(_ids)
        def apply(trainBase: ArffJsonInstancesSource) = f
    }
    
    @serializable
    trait Appendix extends History {
        val projection: Pair[Int, String]
        abstract override def apply(categoryIs: CategoryIs) = super.apply(categoryIs) :+ ProjectionFilter(List(projection._1), projection._2)
    }
}

@serializable
class ProjectionFilter(ids: List[Int]) extends GlobalFilter {
    def applyFilter(source: ArffJsonInstancesSource) = {
        source.map(
            ((inst: ArffJsonInstance) => inst.project(ids)),
            (header: ArffJsonHeader) => new ArffJsonHeader(
                header.relationName, 
                header
                    .attributes
                    .zip(0 until header.attributes.size-1)
                    .filter(m => ids.contains(m._2))
                    .map(_._1)
            )
        )
    }
}









