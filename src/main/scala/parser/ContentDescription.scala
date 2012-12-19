package parser

import common.Path.arffJsonPath
import filter.FilterFactory
import java.io.File

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

case class ContentDescription(val base: String, val set: ContentDescription.Set, val formatHistory: List[FilterFactory]) {
    def filenameAppendix = base + "-" + set.filenameAppendix + (if(!formatHistory.isEmpty) ("_" + formatHistory.map(_.historyAppendix).mkString("_")) else "")
    def file = arffJsonPath / (filenameAppendix + ".json")
    
    def dropLastFilterFactory = ContentDescription(base, set, formatHistory.dropRight(1))
    def addFilterFactory(item: FilterFactory) = ContentDescription(base, set, formatHistory :+ item)
    def toTrain = ContentDescription(base, ContentDescription.TrainSet, formatHistory)
    def toTest = ContentDescription(base, ContentDescription.TestSet, formatHistory)
    def toSet(set: ContentDescription.Set) = ContentDescription(base, set, formatHistory)
    def withFilterFactories(formatHistory: List[FilterFactory]) = ContentDescription(base, set, formatHistory)
    
    override def toString = "ContentDescription(" + base + ", " + set + ", " + formatHistory.map(_.historyAppendix) + ")"
}
























