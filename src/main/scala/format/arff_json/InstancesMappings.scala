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
import parser.ArffJsonInstancesSource
import parser.ContentDescription
import filter.FilterFactory
import filter.StorableFilterFactory
import classifier.Learner
import parser.ContentDescribable

object InstancesMappings {
    def apply(base: ArffJsonInstancesSource with ContentDescribable, target: ContentDescription, targetClassDef: TargetClassDefinition, learner: Learner): ArffJsonInstancesSource with ContentDescribable = {
        if(base.contentDescription == target) {
            base
        } else {
            val instances = if(target.file.exists) Some(ArffJsonInstancesSource(target))
                else None
                
            if(instances.isDefined) {
                instances.get
            } else if(base.contentDescription.set != target.set) {
                def findFirstExistingFileInNewSet(formatHistory: List[FilterFactory]): ArffJsonInstancesSource with ContentDescribable = {
                    val testContentDescription = target.withFilterFactories(formatHistory) 
                    if(testContentDescription.file.exists) ArffJsonInstancesSource(testContentDescription)
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
                            val trainBase = apply(base, target.toTrain.dropLastFilterFactory, targetClassDef, learner)
                            println("mappings - trainBaseSize: " + trainBase.size)
                            println("train " + storable.historyAppendix + " filter with " + trainBase.contentDescription)
                            val filter = storable(trainBase)
                            filter.save(filterFile)
                            filter
                        }
                    }
                    case filterFactory: FilterFactory => {
                        filterFactory(apply(base, target.toTrain.dropLastFilterFactory, targetClassDef, learner))
                    }
                    case _ => throw new RuntimeException("HistoryItem must be a FilterFactory to use mappings")
                }
                
                val underlyingInstances = apply(base, target.dropLastFilterFactory, targetClassDef, learner)
                println("apply " + currentFilterFactory.historyAppendix + " filter on " + target.dropLastFilterFactory)
                val mappedInstances = _filter.applyFilter(underlyingInstances, targetClassDef)
                
                val contentDescribableMappedInstances = new ArffJsonInstancesSource() with ContentDescribable {
                    override def iterator = mappedInstances.iterator
                    def header = mappedInstances.header
                    val contentDescription = underlyingInstances.contentDescription.addFilterFactory(currentFilterFactory)
                }
                
                if(ArffJsonInstances.serializeInstances && !contentDescribableMappedInstances.file.exists()) {
                    print("save " + contentDescribableMappedInstances.contentDescription + "...")
                    contentDescribableMappedInstances.save()
                    println("done")
                }
                contentDescribableMappedInstances
            }
        }
    }
}









