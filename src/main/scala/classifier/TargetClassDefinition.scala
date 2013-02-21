package classifier
import scala.util.matching.Regex

object CategoryIs {
    def top(topClass: Int): CategoryIs = CategoryIs(Some((if(topClass <= 9) "0" else "") + topClass.toString), None, None)
    def top(topClass: String): CategoryIs = CategoryIs(Some(topClass), None, None)
    def topAndMiddle(topClass: String, middleClass: String): CategoryIs = CategoryIs(Some(topClass), Some(middleClass), None)
    def topMiddleAndLeave(topClass: String, middleClass: String, leaveClass: String): CategoryIs = CategoryIs(Some(topClass), Some(middleClass), Some(leaveClass))
    def apply(classDesc: String): CategoryIs = CategoryIs(
        Some(classDesc.substring(0, 2)), 
        (classDesc.substring(2, 3) match {
            case "-" => None
            case x => Some(x.toUpperCase())
        }),
        (classDesc.substring(3, 5).toLowerCase() match {
            case "xx" => None
            case x => Some(classDesc.substring(3, 5).toLowerCase())
        })
    )
}

case class CategoryIs(val topClass: Option[String], val middleClass: Option[String], leaveClass: Option[String]) extends TargetClassDefinition {
    if(topClass.isDefined) assert(topClass.get.length == 2, "top class must have length of 2")
    if(middleClass.isDefined) assert(middleClass.get.length == 1, "middle class must have length of 1")
    if(leaveClass.isDefined) assert(leaveClass.get.length == 2, "leave class must have length of 2")
    
    def apply(classes: List[String]) = {
        !topClass.isDefined || (
            classes.exists(c => {
                c.substring(0, 2) == topClass.get && (!middleClass.isDefined || 
                    c.substring(2, 3) == middleClass.get && (!leaveClass.isDefined || 
                        c.substring(3, 5) == leaveClass.get
                    )
                )
            })
        )
    }
    
    def parent = targetLevel match {
        case 3 => CategoryIs(topClass, middleClass, None)
        case 2 => CategoryIs(topClass, None, None)
        case 1 => CategoryIs(None, None, None)
        case _ => error("There is no parent")
    }
    
    def filenameExtension = (
        if(topClass.isDefined) (if(topClass.get.toInt <= 9) "0" else "") + topClass.get
        else "__"
    ) + (
        if(middleClass.isDefined) middleClass.get
        else "_"
    ) + (
        if(leaveClass.isDefined) leaveClass.get
        else "__"
    )
    
    def targetLevel = 
        if(topClass.isDefined && middleClass.isDefined &&leaveClass.isDefined) 3
        else if(topClass.isDefined && middleClass.isDefined) 2
        else if(topClass.isDefined) 1
        else 0
}

trait TargetClassDefinition extends (List[String] => Boolean) {
    def filenameExtension: String
    def targetLevel: Int
}










