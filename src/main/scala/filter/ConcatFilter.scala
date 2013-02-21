package filter
import parser.ArffJsonInstancesSource
import parser.History
import classifier.TargetClassDefinition
import format.arff_json.ArffJsonHeader
import format.arff_json.ArffJsonInstance

object ConcatFilter {
    def apply(ids: List[Int], concatDesc: String) = new FilterFactory {
        val _ids = ids
        val historyAppendix = "uni-" + concatDesc
        
        @transient lazy val f = new ConcatFilter(_ids)
        def apply(trainBase: ArffJsonInstancesSource) = f
    }
    
    @serializable
    trait Appendix extends History {
        val concat: Pair[List[Int], String]
        abstract override def apply(targetClassDef: TargetClassDefinition) = super.apply(targetClassDef) :+ ConcatFilter(concat._1, concat._2)
    }
}

@serializable
class ConcatFilter(ids: List[Int]) extends GlobalFilter {
    def applyFilter(source: ArffJsonInstancesSource) = {
        source.map(
            (
                (inst: ArffJsonInstance) => inst.union(ids, (
            	    (a: Any, b: Any) => {
            	        (a, b) match {
            	            case (s1: String, s2: String) => s1 + " " + s2
            	            case _ => throw new RuntimeException("only Strings Exception!!")
            	        }
            	    }
            	))
            ),
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