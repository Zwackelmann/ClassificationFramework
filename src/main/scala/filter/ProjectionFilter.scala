package filter
import parser.ArffJsonInstancesSource
import format.arff_json.ArffJsonHeader

class ProjectionFilter(ids: List[Int], val historyAppendix: String) extends GlobalFilter {
    def applyFilter(source: ArffJsonInstancesSource) = {
        println("use projection filter on " + source.contentDescription)
        
        source.map(
            (arffJsonInstances) => arffJsonInstances.map(inst => inst.project(ids)),
            (header) => new ArffJsonHeader(
                header.relationName, 
                header
                    .attributes
                    .zip(0 until header.attributes.size-1)
                    .filter(m => ids.contains(m._2))
                    .map(_._1),
                header.metaAttributes
            ),
            historyAppendix
        )
    }
}