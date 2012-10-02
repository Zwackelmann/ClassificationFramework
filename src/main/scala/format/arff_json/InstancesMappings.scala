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
import filter.FilterFactory
import filter.StorableFilterFactory
import classifier.Learner

object InstancesMappings {
    def apply(base: ArffJsonInstancesSource, target: ContentDescription, targetClassDef: TargetClassDefinition, learner: Learner): ArffJsonInstancesSource = {
        if(base.contentDescription == target) {
            base
        } else {
            val instances = learner.deliverInstances(target)
            if(instances.isDefined) {
                instances.get
            } else if(base.contentDescription.set != target.set) {
                def findFirstExistingFileInNewSet(formatHistory: List[HistoryItem]): ArffJsonInstancesSource = {
                    val testContentDescription = target.withHistory(formatHistory) 
                    if(testContentDescription.file.exists) new ArffJsonInstancesFile(testContentDescription)
                    else if(formatHistory.size > 0) findFirstExistingFileInNewSet(formatHistory.dropRight(1))
                    else throw new RuntimeException("No appropriate file found to jump between sets " + base.contentDescription.set + " and " + target.set)
                }
                apply(findFirstExistingFileInNewSet(target.formatHistory), target, targetClassDef, learner)
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
                
                val currentFilterFactory = target.formatHistory.last
                val filterFile = filterPath / (target.formatHistory.map(_.historyAppendix).mkString("_") + "_filter")
                
                val _filter = currentFilterFactory match {
                    case storable: StorableFilterFactory => {
                        if(Filter.serializeFilters && filterFile.exists) {
                            println("load: " + filterFile)
                            storable.load(filterFile)
                        } else {
                            val trainBase = apply(base, target.toTrain.dropLastHistoryItem, targetClassDef, learner)
                            println("train " + storable.historyAppendix + " filter with " + trainBase.contentDescription)
                            val filter = storable(trainBase)
                            filter.save(filterFile)
                            filter
                        }
                    }
                    case filterFactory: FilterFactory => {
                        filterFactory(apply(base, target.toTrain.dropLastHistoryItem, targetClassDef, learner))
                    }
                    case _ => throw new RuntimeException("HistoryItem must be a FilterFactory to use mappings")
                }
                
                val underlyingInstances = apply(base, target.dropLastHistoryItem, targetClassDef, learner)
                println("apply " + currentFilterFactory.historyAppendix + " filter on " + target.dropLastHistoryItem)
                val mappedInstances = _filter.applyFilter(underlyingInstances, targetClassDef)

                if(ArffJsonInstances.serializeInstances && !mappedInstances.file.exists()) {
                    print("save " + mappedInstances.contentDescription + "...")
                    mappedInstances.save()
                    println("done")
                }
                mappedInstances
            }
        }
    }
}









