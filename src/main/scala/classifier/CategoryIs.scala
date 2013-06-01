package classifier
import scala.util.matching.Regex

object CategoryIsMSC {
    def top(topClass: Int): CategoryIsMSC = CategoryIsMSC(Some((if(topClass <= 9) "0" else "") + topClass.toString), None, None)
    def top(topClass: String): CategoryIsMSC = CategoryIsMSC(Some(topClass), None, None)
    def topAndMiddle(topClass: String, middleClass: String): CategoryIsMSC = CategoryIsMSC(Some(topClass), Some(middleClass), None)
    def topMiddleAndLeave(topClass: String, middleClass: String, leaveClass: String): CategoryIsMSC = CategoryIsMSC(Some(topClass), Some(middleClass), Some(leaveClass))
    def apply(classDesc: String): CategoryIsMSC = CategoryIsMSC(
        Some(classDesc.substring(0, 2)), 
        (if(classDesc.length() >= 3) (classDesc.substring(2, 3) match {
            case "-" => None
            case x => Some(x.toUpperCase())
        }) else None),
        (if(classDesc.length() == 5) (classDesc.substring(3, 5).toLowerCase() match {
            case "xx" => None
            case x => Some(classDesc.substring(3, 5).toLowerCase())
        }) else None)
    )
}

object CategoryIsMSC2 {
    def top(topClass: Int): CategoryIsMSC2 = CategoryIsMSC2(Some((if(topClass <= 9) "0" else "") + topClass.toString), None, None)
    def top(topClass: String): CategoryIsMSC2 = CategoryIsMSC2(Some(topClass), None, None)
    def topAndMiddle(topClass: String, middleClass: String): CategoryIsMSC2 = CategoryIsMSC2(Some(topClass), Some(middleClass), None)
    def topMiddleAndLeave(topClass: String, middleClass: String, leaveClass: String): CategoryIsMSC2 = CategoryIsMSC2(Some(topClass), Some(middleClass), Some(leaveClass))
    def apply(classDesc: String): CategoryIsMSC2 = CategoryIsMSC2(
        Some(classDesc.substring(0, 2)), 
        (if(classDesc.length() >= 3) (classDesc.substring(2, 3) match {
            case "-" => None
            case x => Some(x.toUpperCase())
        }) else None),
        (if(classDesc.length() == 5) (classDesc.substring(3, 5).toLowerCase() match {
            case "xx" => None
            case x => Some(classDesc.substring(3, 5).toLowerCase())
        }) else None)
    )
}

object CategoryIs {
    def oneOf(categories: List[CategoryIs]) = new CategoryIs {
        def matches(classes: List[String]): Option[Boolean] = {
            val x = categories.map(c => c.matches(classes)).flatten
            if(x.isEmpty) {
                None
            } else {
                Some(x.exists(t => t))
            }
        }
        
        override def matchesForTraining(classes: List[String]): Option[Boolean] = {
            val x = categories.map(c => c.matchesForTraining(classes)).flatten
            if(x.isEmpty) {
                None
            } else {
                Some(x.exists(t => t))
            }
        }
        
        override def matchesForTesting(classes: List[String]): Option[Boolean] = {
            val x = categories.map(c => c.matchesForTesting(classes)).flatten
            if(x.isEmpty) {
                None
            } else {
                Some(x.exists(t => t))
            }
        }
        
        def filenameExtension = "one_of_" + categories.map(_.filenameExtension).mkString("_")
    }
    
    def allOf(categories: List[CategoryIs]) = new CategoryIs {
        def matches(classes: List[String]): Option[Boolean] = {
            val x = categories.map(c => c.matches(classes)).flatten
            if(x.isEmpty) {
                Some(false)
            } else {
                Some(x.forall(t => t))
            }
        }
        
        override def matchesForTraining(classes: List[String]): Option[Boolean] = {
            val x = categories.map(c => c.matchesForTraining(classes)).flatten
            if(x.isEmpty) {
                Some(false)
            } else {
                Some(x.forall(t => t))
            }
        }
        
        override def matchesForTesting(classes: List[String]): Option[Boolean] = {
            val x = categories.map(c => c.matchesForTesting(classes)).flatten
            if(x.isEmpty) {
                Some(false)
            } else {
                Some(x.forall(t => t))
            }
        }
        
        def filenameExtension = "all_of_" + categories.map(_.filenameExtension).mkString("_")
    }
}

@serializable
trait CategoryIs {
    def filenameExtension(): String
    def matches(classes: List[String]): Option[Boolean]
    
    def matchesForTesting(classes: List[String]) = matches(classes)
    def matchesForTraining(classes: List[String]) = matches(classes)
}

trait CategorizationHierarchy {
    def parent: CategoryIs with CategorizationHierarchy
    def targetLevel: Int
}

abstract class AbstractCategoryIsMSC(topClass: Option[String], middleClass: Option[String], leafClass: Option[String]) extends CategoryIs with CategorizationHierarchy {
    if(topClass.isDefined) assert(topClass.get.length == 2, "top class must have length of 2")
    if(middleClass.isDefined) assert(middleClass.get.length == 1, "middle class must have length of 1")
    if(leafClass.isDefined) assert(leafClass.get.length == 2, "leave class must have length of 2")
    
    val mscRe = """((\d\d)|(xx)|(__))(([A-Z])|-|_)((\d\d)|(xx)|(__))""".r.pattern
    
    def parent: CategoryIsMSC = targetLevel match {
        case 3 => CategoryIsMSC(topClass, middleClass, None)
        case 2 => CategoryIsMSC(topClass, None, None)
        case 1 => CategoryIsMSC(None, None, None)
        case _ => error("There is no parent")
    }
    
    def filenameExtension = (
        if(topClass.isDefined) topClass.get
        else "__"
    ) + (
        if(middleClass.isDefined) middleClass.get
        else "_"
    ) + (
        if(leafClass.isDefined) leafClass.get
        else "__"
    )
    
    def targetLevel = if(topClass.isDefined && middleClass.isDefined &&leafClass.isDefined) 3
        else if(topClass.isDefined && middleClass.isDefined) 2
        else if(topClass.isDefined) 1
        else 0
    
    override def toString = "CategoryIs(" + filenameExtension + ")"
}

case class CategoryIsMSC(val topClass: Option[String], val middleClass: Option[String], val leafClass: Option[String]) extends AbstractCategoryIsMSC(topClass, middleClass, leafClass) {
    override def matches(classes: List[String]) = {
        // first check if all items are MSC classes
        if(!classes.forall(c => mscRe.matcher(c).matches)) None
        else {
            Some(!topClass.isDefined || (
                classes.exists(c => {
                    c.substring(0, 2) == topClass.get && (!middleClass.isDefined || 
                        c.substring(2, 3) == middleClass.get && (!leafClass.isDefined || 
                            c.substring(3, 5) == leafClass.get
                        )
                    )
                })
            ))
        }
    }
}

case class CategoryIsMSC2(val topClass: Option[String], val middleClass: Option[String], val leafClass: Option[String]) extends AbstractCategoryIsMSC(topClass, middleClass, leafClass) {
    override def matches(classes: List[String]) = matchesForTraining(classes)
    override def matchesForTesting(classes: List[String]) = {
        // first check if all items are MSC classes
        if(!classes.forall(c => mscRe.matcher(c).matches)) None
        else {
            Some(!topClass.isDefined || (
                classes.exists(c => {
                    c.substring(0, 2) == topClass.get && (!middleClass.isDefined || 
                        c.substring(2, 3) == middleClass.get && (!leafClass.isDefined || 
                            c.substring(3, 5) == leafClass.get
                        )
                    )
                })
            ))
        }
    }
    
    override def matchesForTraining(classes: List[String]) = {
        if(classes.isEmpty) None
        else if(!mscRe.matcher(classes.head).matches) None
        else {
            Some(!topClass.isDefined || (
                classes.head.substring(0, 2) == topClass.get && (
                    !middleClass.isDefined || classes.head.substring(2, 3) == middleClass.get && (
                        !leafClass.isDefined || classes.head.substring(3, 5) == leafClass.get
                    )
                )
            ))
        }
    }
}






