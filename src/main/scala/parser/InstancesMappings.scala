package parser

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

object InstancesMappings {
    def nominalMapping(projectId: Int) = new InstancesMappings {
        def filter(filterName: String) = {
            filterName match {
                case project if project == "project-" + projectId => ((_: ArffJsonInstancesSource) => new ProjectionFilter(List(projectId), "project-" + projectId), None)
                case "nominalized" => ((trainBase: ArffJsonInstancesSource) => {
                    val filter = new NominalizeFilter.Conf1("nominalized")
                    filter.expandDict(trainBase)
                    filter
                }, Some((filterFile: File) => NominalizeFilter.load(filterFile)))
                case "flattened" => (
                    (_: ArffJsonInstancesSource) => new FlattenFilter,
                    None
                )
            }
        }
    }
    
    def instancesMappings1(projectId: Int, numLsiDims: Int) = new InstancesMappings {
        def filter(filterName: String) = {
            filterName match {
                case project if project == "project-" + projectId => ((_: ArffJsonInstancesSource) => new ProjectionFilter(List(projectId), "project-" + projectId), None) 
                    
                case "vector-conf4" => ((trainBase: ArffJsonInstancesSource) => {
                    val filter = new VectorFromDictFilter.Conf4
                    filter.buildDict(trainBase)
                    filter
                }, Some((filterFile: File) => {
                    VectorFromDictFilter.load(filterFile)
                }))
                case "tf-idf" => (
                    (trainBase: ArffJsonInstancesSource) => new TfIdfFilter(trainBase, "tf-idf"),
                    Some((filterFile: File) => TfIdfFilter.load(filterFile))
                )
                case "normalized" => (
                    (_: ArffJsonInstancesSource) => new NormalizeVectorFilter,
                    None
                )
                case lsi if lsi == ("lsi-" + numLsiDims) => (
                    (trainBase: ArffJsonInstancesSource) => {
                        val filter = new GensimLsiFilter(numLsiDims)
                        filter.builtModel(trainBase)
                        filter
                    },
                    Some((filterFile: File) => GensimLsiFilter.load(filterFile))
                )
                case _ => throw new RuntimeException(filterName + " is no valid filter name")
            }
        }
    }
    
    def orMappings(projectId: Int, numOrDims: Int) = new InstancesMappings {
        def filter(filterName: String) = {
            filterName match {
                case project if project == "project-" + projectId => ((_: ArffJsonInstancesSource) => new ProjectionFilter(List(projectId), "project-" + projectId), None) 
                    
                case "vector-conf4" => ((trainBase: ArffJsonInstancesSource) => {
                    val filter = new VectorFromDictFilter.Conf4
                    filter.buildDict(trainBase)
                    filter
                }, Some((filterFile: File) => {
                    VectorFromDictFilter.load(filterFile)
                }))
                case or if or == ("or-" + numOrDims) => (
                    (trainBase: ArffJsonInstancesSource) => {
                        new OddsRatioFilter(trainBase, numOrDims, "or-" + numOrDims)
                    },
                    Some((filterFile: File) => OddsRatioFilter.load(filterFile))
                )
                case _ => throw new RuntimeException(filterName + " is no valid filter name")
            }
        }
    }
}

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
    
    /*def instancesMappings1(
            numLsiDims: Int
    ) = {
        def fun(base: ArffJsonInstancesSource, target: ContentDescription): ArffJsonInstancesSource = {
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
                fun(findFirstExistingFileInNewSet(target.formatHistory), target)
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
                filterToApply match {
                    case "vector-conf4" => {
                        val filter = if(!filterFile.exists()) {
                            val filter = new VectorFromDictFilter.Conf4
                            filter.buildDict(fun(base, target.toTrain.dropLastHistoryItem))
                            filter.save(filterFile)
                            filter
                        } else {
                            VectorFromDictFilter.load(filterFile)
                        }
                        
                        filter.applyFilter(fun(base, target.dropLastHistoryItem))
                    }
                    case "tf-idf" => {
                        val filter = if(!filterFile.exists()) {
                            val filter = new TfIdfFilter(
                                fun(base, target.toTrain.dropLastHistoryItem), "tf-idf"
                            )
                            filter.save(filterFile)
                            filter
                        } else {
                            TfIdfFilter.load(filterFile)
                        }
                        filter.applyFilter(fun(base, target.dropLastHistoryItem))
                    }
                    case "normalized" => {
                        val filter = new NormalizeVectorFilter
                        filter.applyFilter(fun(base, target.dropLastHistoryItem))
                    }
                    case lsi if lsi == ("lsi-" + numLsiDims) => {
                        val filter = if(!filterFile.exists()) {
                            val filter = new GensimLsiFilter(numLsiDims)
                            filter.builtModel(fun(base, target.toTrain.dropLastHistoryItem))
                            filter.save(filterFile)
                            filter
                        } else {
                            GensimLsiFilter.load(filterFile)
                        }
                        filter.applyFilter(fun(base, target.dropLastHistoryItem))
                    }
                    case _ => throw new RuntimeException(target + " is no valid content description for instancesMappings")
                }
            }
        }
        
        fun _
    }*/
}