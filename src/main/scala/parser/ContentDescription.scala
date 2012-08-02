package parser

import common.Path.arffJsonPath

case class ContentDescription(val base: String, val set: String, val formatHistory: List[String]) {
    def filenameAppendix = base + "-" + set + (if(!formatHistory.isEmpty) ("_" + formatHistory.mkString("_")) else "")
    def file = arffJsonPath / (filenameAppendix + ".json")
    
    def dropLastHistoryItem = ContentDescription(base, set, formatHistory.dropRight(1))
    def addHistoryItem(item: String) = ContentDescription(base, set, formatHistory :+ item)
    def toTrain = ContentDescription(base, "train", formatHistory)
    def toTest = ContentDescription(base, "test", formatHistory)
    def withHistory(formatHistory: List[String]) = ContentDescription(base, set, formatHistory)
}