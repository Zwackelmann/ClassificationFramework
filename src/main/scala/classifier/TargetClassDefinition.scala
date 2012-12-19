package classifier
import scala.util.matching.Regex

@serializable
object TopClassIs {
    def apply(targetClass: String) = new TopClassIs(targetClass)
}

@serializable
object TopAndMiddleClassIs {
    def apply(topClass: String, middleClass: String) = new TopAndMiddleClassIs(topClass, middleClass)
}

@serializable
class TopClassIs(val targetClass: String) extends TargetClassDefinition {
    def apply(classes: List[String]) = classes.exists(_.substring(0, 2).toInt == targetClass.toInt)
    override def toString = "TopClassIs(" + targetClass + ")"
    override def filenameExtension = "tg-" + targetClass + "XXXX"
}

@serializable
class TopAndMiddleClassIs(val topClass: String, val middleClass: String) extends TargetClassDefinition {
    def apply(classes: List[String]) = classes.exists(c => c.substring(0, 2).toInt == topClass.toInt && c.substring(2, 3) == middleClass)
    override def toString = "TopAndMiddleClassIs(" + topClass + ", " + middleClass + ")"
    override def filenameExtension = "tg-" + topClass + middleClass + "XX"
}

trait TargetClassDefinition extends (List[String] => Boolean) {
    def filenameExtension: String
}










