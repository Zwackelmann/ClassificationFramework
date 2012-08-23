package filter
import parser.ArffJsonInstancesSource
import format.arff_json.ArffJsonHeader
import format.arff_json.HistoryItem
import format.arff_json.ArffJsonInstance

object ProjectionFilter {
    def apply(ids: List[Int], projectionDesc: String) = new FilterFactory {
        def apply(trainBase: ArffJsonInstancesSource) = new ProjectionFilter(ids, this)
        val historyAppendix = "proj-" + projectionDesc
    }
}

class ProjectionFilter(ids: List[Int], val historyAppendix: HistoryItem) extends GlobalFilter {
    def applyFilter(source: ArffJsonInstancesSource) = {
        source.map(
            (arffJsonInstances: Iterator[ArffJsonInstance]) => arffJsonInstances.map(inst => inst.project(ids)),
            (header: ArffJsonHeader) => new ArffJsonHeader(
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