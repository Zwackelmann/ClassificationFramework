package filter
import parser.ArffJsonInstancesSource
import parser.History
import format.arff_json.ArffJsonHeader
import format.arff_json.ArffJsonInstance
import classifier.CategoryIs

object ConcatFilter {
    def apply(ids: List[Int]) = new FilterFactory {
        val _ids = ids
        val historyAppendix = "uni"
        
        @transient lazy val f = new ConcatFilter(_ids)
        def apply(trainBase: ArffJsonInstancesSource) = f
    }
    
    def apply(ids: List[Int], concatDesc: String) = new FilterFactory {
        val _ids = ids
        val historyAppendix = "uni-" + concatDesc
        
        @transient lazy val f = new ConcatFilter(_ids)
        def apply(trainBase: ArffJsonInstancesSource) = f
    }
    
    trait Appendix extends History with Serializable {
        val concat: Pair[List[Int], String]
        abstract override def apply(categoryIs: CategoryIs) = super.apply(categoryIs) :+ ConcatFilter(concat._1, concat._2)
    }
}

class ConcatFilter(ids: List[Int]) extends GlobalFilter with Serializable {
    def applyFilter(source: ArffJsonInstancesSource) = {
        source.map(
            (
                (inst: ArffJsonInstance) => inst.union(ids, (
            	    (a: Any, b: Any) => {
            	        (a, b) match {
            	            case (s1: String, s2: String) => s1 + " " + s2
            	            case _ => {
            	                throw new RuntimeException("only Strings Exception!!")
            	            }
            	        }
            	    }
            	))
            ),
            (header: ArffJsonHeader) => {
                if(header.explicitAttributes) {
                    ArffJsonHeader(
                        header.relationName, 
                        header
                            .attributes
                            .zip(0 until header.attributes.size-1)
                            .filter(m => ids.contains(m._2))
                            .map(_._1)
                    )
                } else {
                    ArffJsonHeader(
                        header.relationName, 
                        ids.size
                    )
                }
            }
                
        )
    }
    
    override val trainingParams = None
}







