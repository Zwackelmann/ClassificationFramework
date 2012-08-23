package parser

import common.Path.arffJsonPath
import format.arff_json.HistoryItem
import format.arff_json.HistoryItem

object ContentDescription {
    trait Set {
        def filenameAppendix: String 
    }
    
    case object TestSet extends Set {
        override def filenameAppendix = "test"
    }
    
    case object TrainSet extends Set {
        override def filenameAppendix = "train"
    }
}

case class ContentDescription(val base: String, val set: ContentDescription.Set, val formatHistory: List[HistoryItem]) {
    def filenameAppendix = base + "-" + set.filenameAppendix + (if(!formatHistory.isEmpty) ("_" + formatHistory.map(_.historyAppendix).mkString("_")) else "")
    def file = arffJsonPath / (filenameAppendix + ".json")
    
    def dropLastHistoryItem = ContentDescription(base, set, formatHistory.dropRight(1))
    def addHistoryItem(item: HistoryItem) = ContentDescription(base, set, formatHistory :+ item)
    def toTrain = ContentDescription(base, ContentDescription.TrainSet, formatHistory)
    def toTest = ContentDescription(base, ContentDescription.TestSet, formatHistory)
    def toSet(set: ContentDescription.Set) = ContentDescription(base, set, formatHistory)
    def withHistory(formatHistory: List[HistoryItem]) = ContentDescription(base, set, formatHistory)
    
    override def toString = "ContentDescription(" + base + ", " + set + ", " + formatHistory.map(_.historyAppendix) + ")"
}









