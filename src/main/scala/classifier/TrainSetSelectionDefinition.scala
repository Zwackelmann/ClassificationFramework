package classifier

sealed trait TrainSetSelectionDefinition {
    def filenameAppendix: String
}

case class FixedTrainSetSelection(val targetInstances: Option[Int], val otherInstances: Option[Int]) extends TrainSetSelectionDefinition {
    def filenameAppendix = ("ftss-" + 
        (if(targetInstances.isDefined) targetInstances.get else "all") + "-" + 
        (if(otherInstances.isDefined) otherInstances.get else "all")
    )
}

case object NoTrainSetSelection extends TrainSetSelectionDefinition {
    def filenameAppendix = "all"
}

case class BalancedTrainSetSelection(val maxInstPerGroup: Option[Int]) extends TrainSetSelectionDefinition {
    def filenameAppendix = "btss-" + (if(maxInstPerGroup.isDefined) maxInstPerGroup.get else "all") 
}

case class MaxForEachSet(val maxInstPositives: Option[Int], maxInstNegatives: Option[Int]) extends TrainSetSelectionDefinition {
    def filenameAppendix = "mes-" + (if(maxInstPositives.isDefined) maxInstPositives.get else "all") + "-" + (if(maxInstNegatives.isDefined) maxInstNegatives.get else "all")
}

case class PrioritizeMainClass(val maxTotal: Int) extends TrainSetSelectionDefinition {
    def filenameAppendix = "main-" + maxTotal
}



