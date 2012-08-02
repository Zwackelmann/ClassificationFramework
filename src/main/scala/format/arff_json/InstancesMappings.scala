package format.arff_json

import common.Path.filterPath
import filter.VectorFromDictFilter
import filter.TfIdfFilter
import filter.NormalizeVectorFilter
import external.GensimLsiFilter
import java.io.File
import filter.Filter
import filter.ProjectionFilter
import filter.NominalValueFromDictFilter
import classifier.TargetClassDefinition
import filter.feature_scoreing.OddsRatio
import filter.OddsRatioFilter
import filter.FlattenFilter
import filter.NominalizeFilter
import parser.ArffJsonInstancesSource
import parser.ContentDescription
import parser.ArffJsonInstancesFile

trait InstancesMappings {
    def apply(base: ArffJsonInstancesSource, target: ContentDescription, targetClassDef: TargetClassDefinition): ArffJsonInstancesSource = {
            if(base.contentDescription == target) {
                base
            } else if(target.file.exists) {
                new ArffJsonInstancesFile(target)
            } else if(base.contentDescription.set != target.set) {
                def findFirstExistingFileInNewSet(formatHistory: List[String]): ArffJsonInstancesSource = {
                    val testContentDescription = target.withHistory(formatHistory) 
                    if(testContentDescription.file.exists) new ArffJsonInstancesFile(testContentDescription)
                    else if(formatHistory.size > 0) findFirstExistingFileInNewSet(formatHistory.dropRight(1))
                    else throw new RuntimeException("No appropriate file found to jump between sets " + base.contentDescription.set + " and " + target.set)
                }
                apply(findFirstExistingFileInNewSet(target.formatHistory), target, targetClassDef)
            } else {
                if(base.contentDescription.formatHistory.size > target.formatHistory.size) {
                    throw new RuntimeException("TODO?")
                }
                
                // check if the first history items are equal
                if(!target.formatHistory
                    .take(base.contentDescription.formatHistory.size)
                    .zip(base.contentDescription.formatHistory)
                    .map(h => h._1 == h._2)
                    .forall(b => b)
                ) {
                    throw new RuntimeException("Inconsistent history")
                }
                
                val filterToApply = target.formatHistory.last
                
                val filterFile = filterPath / (target.base + "_" + filterToApply + "_filter")
                
                val _filter = filter(filterToApply) match {
                    case(filterFun: (ArffJsonInstancesSource => Filter), Some(fileFun: (File => Filter))) => { 
                        val filter = if(!filterFile.exists()) {
                            filterFun(apply(base, target.toTrain.dropLastHistoryItem, targetClassDef))
                        } else {
                            fileFun(filterFile)
                        }
                        filter.save(filterFile)
                        filter
                    }
                    
                    case(filterFun: (ArffJsonInstancesSource => Filter), None) => {
                        filterFun(apply(base, target.toTrain.dropLastHistoryItem, targetClassDef))
                    }
                }
                
                _filter.applyFilter(apply(base, target.dropLastHistoryItem, targetClassDef), targetClassDef)
            }
        }
    
    def filter(filterName: String): Pair[ArffJsonInstancesSource => Filter, Option[File => Filter]]
}