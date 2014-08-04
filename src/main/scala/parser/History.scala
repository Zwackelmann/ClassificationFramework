package parser
import classifier.CategoryIs
import filter.FilterFactory

class History extends (CategoryIs => List[FilterFactory]) with Serializable {
    def apply(targetClassDef: CategoryIs): List[FilterFactory] = List()
}