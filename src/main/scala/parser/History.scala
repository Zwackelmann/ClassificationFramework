package parser
import classifier.CategoryIs
import filter.FilterFactory

@serializable
class History extends (CategoryIs => List[FilterFactory]) {
    def apply(targetClassDef: CategoryIs): List[FilterFactory] = List()
}