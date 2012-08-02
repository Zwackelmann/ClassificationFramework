package filter
import parser.ArffJsonInstancesSource
import format.arff_json.ArffJsonInstance
import format.arff_json.DenseArffJsonInstance
import format.arff_json.ArffJsonHeader

class FlattenFilter extends GlobalFilter {
    val historyAppendix = "flattened"
    
    def applyFilter(source: ArffJsonInstancesSource) = {
        require(source.header.attributes.size == 1)
        println("use flatten filter on " + source.contentDescription)
        
        source.map(
            elemFun = instances => {
                instances.flatMap(inst => {
                    inst.data.map(d => new DenseArffJsonInstance(inst.id, inst.mscClasses, List(d)))
                })
            },
            headerFun = header => header,
            historyAppendix = historyAppendix
        )
    } 
}