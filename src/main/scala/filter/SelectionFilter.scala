package filter
import parser.ArffJsonInstancesSource
import format.arff_json.ArffJsonHeader
import format.arff_json.ArffJsonInstance
import parser.History
import classifier.TargetClassDefinition
import classifier.CategoryIs

@serializable
class SelectionFilter(filterFun: ArffJsonInstance => Boolean) extends GlobalFilter {
    def applyFilter(source: ArffJsonInstancesSource) = source.filter(filterFun)
}

object CategorySelectionFilter {
    def apply(targetClass: TargetClassDefinition) = new FilterFactory {
        val historyAppendix = "sel-" + targetClass.filenameExtension
        def apply(trainBase: ArffJsonInstancesSource) = new CategorySelectionFilter(targetClass)
    }
    
    @serializable
    trait Appendix extends History {
        val selection: TargetClassDefinition
        abstract override def apply(targetClassDef: TargetClassDefinition) = super.apply(targetClassDef) :+ CategorySelectionFilter(selection)
    }
}

class CategorySelectionFilter(val targetClass: TargetClassDefinition) extends SelectionFilter((inst: ArffJsonInstance) => targetClass(inst.categories))




