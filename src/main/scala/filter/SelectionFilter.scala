package filter
import parser.ArffJsonInstancesSource
import format.arff_json.ArffJsonHeader
import format.arff_json.ArffJsonInstance
import parser.History
import classifier.CategoryIs
import parser.ContentDescribable

@serializable
abstract class SelectionFilter(filterFun: ArffJsonInstance => Boolean) extends GlobalFilter {
    def applyFilter(source: ArffJsonInstancesSource) = source.filter(filterFun)
}

object CategorySelectionFilter {
    def apply(categoryIs: CategoryIs) = new FilterFactory {
        val historyAppendix = "sel-" + categoryIs.filenameExtension
        def apply(trainBase: ArffJsonInstancesSource) = new CategorySelectionFilter(categoryIs) {
            override val trainingParams = None
        }
    }
    
    @serializable
    trait Appendix extends History {
        val selection: CategoryIs
        abstract override def apply(categoryIs: CategoryIs) = super.apply(categoryIs) :+ CategorySelectionFilter(selection)
    }
}

abstract class CategorySelectionFilter(val categoryIs: CategoryIs) extends SelectionFilter(
    (inst: ArffJsonInstance) => {
        val isTarget = categoryIs.matchesForTraining(inst.categories)
        isTarget.isDefined && isTarget.get
    }
)




