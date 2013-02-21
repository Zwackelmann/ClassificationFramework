package parser
import classifier.TargetClassDefinition
import filter.FilterFactory

@serializable
class History extends (TargetClassDefinition => List[FilterFactory]) {
    def apply(targetClassDef: TargetClassDefinition): List[FilterFactory] = List()
}