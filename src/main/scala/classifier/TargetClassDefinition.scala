package classifier
import scala.util.matching.Regex

@serializable
object TopClassIs {
    def apply(targetClass: String) = new TopClassIs(targetClass)
    val re = """TopClassIs\(([^)]+)\)""".r
    val description = "TopClassIs"
}

@serializable
class TopClassIs(val targetClass: String) extends TargetClassDefinition {
    def apply(classes: List[String]) = classes.exists(_.substring(0, 2) == targetClass)
    override def toString = "TopClassIs(" + targetClass + ")"
    override def filenameExtension = "top-class-is-" + targetClass
}

@serializable
object Global extends TargetClassDefinition {
    def apply(classes: List[String]) = true
    def filenameExtension = "global"
}

trait TargetClassDefinition extends (List[String] => Boolean) {
    def filenameExtension: String
}










