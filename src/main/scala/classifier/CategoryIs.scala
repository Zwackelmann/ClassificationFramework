package classifier
import scala.util.matching.Regex

object CategoryIsMscSome {
    def top(topClass: Int): CategoryIsMscSome = CategoryIsMscSome(Some((if(topClass <= 9) "0" else "") + topClass.toString), None, None)
    def top(topClass: String): CategoryIsMscSome = CategoryIsMscSome(Some(topClass), None, None)
    def topAndMiddle(topClass: String, middleClass: String): CategoryIsMscSome = CategoryIsMscSome(Some(topClass), Some(middleClass), None)
    def topMiddleAndLeave(topClass: String, middleClass: String, leaveClass: String): CategoryIsMscSome = CategoryIsMscSome(Some(topClass), Some(middleClass), Some(leaveClass))
    def apply(classDesc: String): CategoryIsMscSome = CategoryIsMscSome(
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

object CategoryIsMscMain {
    def top(topClass: Int): CategoryIsMscMain = CategoryIsMscMain(Some((if(topClass <= 9) "0" else "") + topClass.toString), None, None)
    def top(topClass: String): CategoryIsMscMain = CategoryIsMscMain(Some(topClass), None, None)
    def topAndMiddle(topClass: String, middleClass: String): CategoryIsMscMain = CategoryIsMscMain(Some(topClass), Some(middleClass), None)
    def topMiddleAndLeave(topClass: String, middleClass: String, leaveClass: String): CategoryIsMscMain = CategoryIsMscMain(Some(topClass), Some(middleClass), Some(leaveClass))
    def apply(classDesc: String): CategoryIsMscMain = CategoryIsMscMain(
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

object CategoryIsMscOnly {
    def top(topClass: Int): CategoryIsMscOnly = CategoryIsMscOnly(Some((if(topClass <= 9) "0" else "") + topClass.toString), None, None)
    def top(topClass: String): CategoryIsMscOnly = CategoryIsMscOnly(Some(topClass), None, None)
    def topAndMiddle(topClass: String, middleClass: String): CategoryIsMscOnly = CategoryIsMscOnly(Some(topClass), Some(middleClass), None)
    def topMiddleAndLeave(topClass: String, middleClass: String, leaveClass: String): CategoryIsMscOnly = CategoryIsMscOnly(Some(topClass), Some(middleClass), Some(leaveClass))
    def apply(classDesc: String): CategoryIsMscOnly = CategoryIsMscOnly(
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


trait CategoryIs extends Serializable {
    def filenameExtension(): String
    def matches(classes: List[String]): Option[Boolean]
    
    def matchesForTesting(classes: List[String]) = matches(classes)
    def matchesForTraining(classes: List[String]) = matches(classes)
}

trait CategorizationHierarchy {
    def parent: CategoryIs with CategorizationHierarchy
    def targetLevel: Int
}

trait CategoryIsMsc extends CategoryIs with CategorizationHierarchy {
    val topClass: Option[String]
    val middleClass: Option[String]
    val leafClass: Option[String]
    
    if(topClass.isDefined) assert(topClass.get.length == 2, "top class must have length of 2")
    if(middleClass.isDefined) assert(middleClass.get.length == 1, "middle class must have length of 1")
    if(leafClass.isDefined) assert(leafClass.get.length == 2, "leave class must have length of 2")
    
    val mscRe = """((\d\d)|(xx)|(__))(([A-Z])|-|_)((\d\d)|(xx)|(__))""".r.pattern
    
    def parent: CategoryIsMsc = targetLevel match {
        case 3 => this match {
            case some: CategoryIsMscSome => CategoryIsMscSome(topClass, middleClass, None)
            case only: CategoryIsMscOnly => CategoryIsMscOnly(topClass, middleClass, None)
            case main: CategoryIsMscMain => CategoryIsMscMain(topClass, middleClass, None)
        }
        case 2 => this match {
            case some: CategoryIsMscSome => CategoryIsMscSome(topClass, None, None)
            case only: CategoryIsMscOnly => CategoryIsMscOnly(topClass, None, None)
            case main: CategoryIsMscMain => CategoryIsMscMain(topClass, None, None)
        }
        case 1 =>  this match {
            case some: CategoryIsMscSome => CategoryIsMscSome(None, None, None)
            case only: CategoryIsMscOnly => CategoryIsMscOnly(None, None, None)
            case main: CategoryIsMscMain => CategoryIsMscMain(None, None, None)
        }
        case _ => error("There is no parent")
    }
    
    def filenameExtension = "so-" + (
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

case class CategoryIsMscSome(val topClass: Option[String], val middleClass: Option[String], val leafClass: Option[String]) extends CategoryIsMsc {
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

case class CategoryIsMscMain(val topClass: Option[String], val middleClass: Option[String], val leafClass: Option[String]) extends CategoryIsMsc {
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
    
    override def filenameExtension = "mc-" + ((
        if(topClass.isDefined) topClass.get
        else "__"
    ) + (
        if(middleClass.isDefined) middleClass.get
        else "_"
    ) + (
        if(leafClass.isDefined) leafClass.get
        else "__"
    ))
    
    override def matchesForTraining(classes: List[String]) = {
        if(classes.isEmpty) Some(false)
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

case class CategoryIsMscOnly(val topClass: Option[String], val middleClass: Option[String], val leafClass: Option[String]) extends CategoryIsMsc {
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
    
    override def filenameExtension = "oy-" + ((
        if(topClass.isDefined) topClass.get
        else "__"
    ) + (
        if(middleClass.isDefined) middleClass.get
        else "_"
    ) + (
        if(leafClass.isDefined) leafClass.get
        else "__"
    ))
    
    override def matchesForTraining(classes: List[String]) = {
        if(classes.isEmpty) Some(false)
        else if(!mscRe.matcher(classes.head).matches) None
        else {
            Some(!topClass.isDefined || (
                classes.forall(cl => cl.substring(0, 2) == topClass.get) && (
                    !middleClass.isDefined || classes.forall(cl => cl.substring(2, 3) == middleClass.get) && (
                        !leafClass.isDefined || (classes.forall(cl => cl.substring(3, 5) == leafClass.get))
                    )
                )
            ))
        }
    }
}





