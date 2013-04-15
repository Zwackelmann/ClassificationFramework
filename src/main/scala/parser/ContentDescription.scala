package parser

import common.Path.arffJsonPath
import filter.FilterFactory
import java.io.File
import common.FileManager
import FileManager.Protocol._

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
    
    case object TuningSet extends Set {
        override def filenameAppendix = "tuning"
    }
}

case class ContentDescription(val base: String, val set: ContentDescription.Set, val formatHistory: List[(FilterFactory, ContentDescription)]) {
    def filename: String = base + "-" + set.filenameAppendix + (if(!formatHistory.isEmpty) ("_" + (formatHistory.map(_._1.historyAppendix).mkString("_")) + "__" + (formatHistory.map(_._2.filename)).mkString("_")) else "")
    def fullFilename = arffJsonPath / (filename + ".json")
    def fileExists = (FileManager !? FileExists(fullFilename)) match {
        case Exists(b) => b
        case Error(msg) => throw new RuntimeException(msg)
    }
    
    def dropLastFilterFactory = ContentDescription(base, set, formatHistory.dropRight(1))
    def addHistoryItem(item: (FilterFactory, ContentDescription)) = ContentDescription(base, set, formatHistory :+ item)
    def toTrain = ContentDescription(base, ContentDescription.TrainSet, formatHistory)
    def toTest = ContentDescription(base, ContentDescription.TestSet, formatHistory)
    def toTuning = ContentDescription(base, ContentDescription.TuningSet, formatHistory)
    def toSet(set: ContentDescription.Set) = ContentDescription(base, set, formatHistory)
    def withHistoryFactories(formatHistory: List[(FilterFactory, ContentDescription)]) = ContentDescription(base, set, formatHistory)
    
    override def toString = "ContentDescription(" + base + ", " + set + ", " + formatHistory.map(_._1.historyAppendix) + ", " + formatHistory.map(_._2.toString) + ")"
}
























