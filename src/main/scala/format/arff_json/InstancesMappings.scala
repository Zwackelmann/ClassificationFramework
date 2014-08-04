package format.arff_json

import common.Path.filterPath
import filter.VectorFromDictFilter
import filter.TfIdfFilter
import filter.NormalizeVectorFilter
import external.GensimLsiFilter
import java.io.File
import filter.Filter
import filter.ProjectionFilter
import classifier.CategoryIs
import filter.feature_scoreing.OddsRatio
import filter.OddsRatioFilter
import parser.ArffJsonInstancesSource
import parser.ContentDescription
import filter.FilterFactory
import classifier.Learner
import parser.ContentDescribable
import filter.Loadable
import parser.ContentDescription$
import common.FileManager
import FileManager.Protocol._
import scala.concurrent._
import ExecutionContext.Implicits.global

object InstancesMappings {
    import common.Common.verbosity
    
    def foldHistoryIntoCd(base: ContentDescription, filters: List[FilterFactory]) = 
        ((base /: filters)((oldCd, ff) => oldCd.addHistoryItem((ff, oldCd))))
    
    def getInstancesAndFilter(base: ArffJsonInstancesSource, filterFactory: FilterFactory, cat: CategoryIs) = {
        val filter = (base, filterFactory) match {
            case (contentDescribable: ContentDescribable, loadable: Loadable[_]) => {
                val filterFullFilename = Filter.fullFilename(contentDescribable.contentDescription, filterFactory.historyAppendix)
                
                val filter = (FileManager !? CreateOrReceiveFile(filterFullFilename)) match {
                    case AcceptReceiveFile(file) => {
                        if(verbosity >= 2) println("load: " + filterFullFilename)
                        loadable.load(filterFullFilename) match {
                            case Some(filter: Filter) => filter
                            case None => {
                                if(verbosity >= 1) println("failed to load " + filterFullFilename + " => delete and regenerate")
                                try {
                                    file.delete()
                                } catch {
                                    case ex: Throwable => 
                                }
                                
                                (FileManager !? CreateOrReceiveFile(filterFullFilename)) match {
                                    case AcceptReceiveFile(file) => loadable.load(filterFullFilename).get
                                    case AcceptCreateFile(file) => {
                                        val filter = loadable.apply(base)
                                        loadable.save(filter, filterFullFilename)
                                        filter
                                    }
                                }
                            }
                        }
                    }
                    case AcceptCreateFile(fileHandle) => {
                        if(verbosity >= 2) println("train: " + filterFullFilename)
                        val filter = loadable.apply(base)
                        println("save filter as " + filterFullFilename)
                        loadable.save(filter, filterFullFilename)
                        filter
                    }
                }
                filter
            }
            case (base, filterFactory) => {
                filterFactory.apply(base)
            }
        }
        
        val mappedInst: ArffJsonInstancesSource = base match {
            case contentDescribable: ContentDescribable => {
                val cd = contentDescribable.contentDescription
                val mappedInst = ArffJsonInstancesSource(ContentDescription(cd.base, cd.set, cd.formatHistory :+ (filterFactory, cd)))
                
                if(mappedInst.isDefined) {
                    if(verbosity >= 2) println("instances already saved => receive instances: " + contentDescribable.contentDescription)
                    mappedInst.get
                } else {
                    val mappedInst = filter.applyFilter(base, cat)
                    val contentDescribableMappedInstances = new ArffJsonInstancesSource() with ContentDescribable {
                        override def iterator = mappedInst.iterator
                        def header = mappedInst.header
                        val contentDescription = cd.addHistoryItem((filterFactory, cd))
                    }
                    if(verbosity >= 2) print("saving " + contentDescribableMappedInstances.contentDescription + "...")
                    contentDescribableMappedInstances.save
                    // free some file handle here !!!!!!
                    if(verbosity >= 2) println("done")
                    contentDescribableMappedInstances
                }
            }
            case inst => {
                filter.applyFilter(inst, cat)
            }
        }
        
        (mappedInst, filter)
    }
    
    def getInstancesAndFilters(base: ArffJsonInstancesSource, targetFilters: List[FilterFactory], cat: CategoryIs): (ArffJsonInstancesSource, List[Filter]) = {
        val x = if(targetFilters.isEmpty) {
            (base, List())
        } else if(targetFilters.size == 1) {
            val filterFactory = targetFilters.last
            val (mappedInst, filter) = getInstancesAndFilter(base, filterFactory, cat)
            (mappedInst, List(filter))
        } else {
            val (baseInst, appliedFilters) = getInstancesAndFilters(base, targetFilters.dropRight(1), cat)
            val filterFactory = targetFilters.last
            val (mappedInst, filter) = getInstancesAndFilter(baseInst, filterFactory, cat)
            (mappedInst, appliedFilters :+ filter)
        }
        
        x
    }
    
    def getInstances(trainBase: ArffJsonInstancesSource, targetFilters: List[FilterFactory], targetInstBase: ArffJsonInstancesSource, cat: CategoryIs): ArffJsonInstancesSource = {
        (trainBase, targetInstBase) match {
            case (describableTrainBase: ContentDescribable, describableInstBase: ContentDescribable) => {
                val targetCd = {
                    val c = (describableTrainBase.contentDescription /: targetFilters)((oldCd, ff) => oldCd.addHistoryItem((ff, oldCd)))
                    ContentDescription(describableInstBase.contentDescription.base, describableInstBase.contentDescription.set, c.formatHistory)
                }
                
                if(verbosity >= 2) println("Train base and target base are content describable => check if file exists (" + targetCd.fullFilename + ")")
                
                (FileManager !? CreateOrReceiveFile(targetCd.fullFilename)) match {
                    case AcceptCreateFile(fileHandle) => {
                        if(verbosity >= 2) println("no => load filters and map instances")
                        
                        val (mappedInst, filters) = getInstancesAndFilters(trainBase, targetFilters, cat)
                        if(describableTrainBase.contentDescription.base == describableInstBase.contentDescription.base && 
                                describableTrainBase.contentDescription.set == describableInstBase.contentDescription.set
                        ) {
                            if(verbosity >= 2) println("train base and target base are the same => just return mappedInstances")
                            fileHandle.close
                            mappedInst
                        } else {
                            val (mappedInst, _) = (((describableInstBase, describableTrainBase.contentDescription) /: (filters zip targetFilters))((oldInstWithTrainCd, filterWithFactory) => {
                                    val (filter, factory) = filterWithFactory
                                    val (oldInst, trainBaseCd) = oldInstWithTrainCd
                                    
                                    val mappedInst = filter.applyFilter(oldInst, cat)
                                    
                                    val contentDescribableMappedInst = new ArffJsonInstancesSource() with ContentDescribable {
                                        override def iterator = mappedInst.iterator
                                        def header = mappedInst.header
                                        val contentDescription = oldInst.contentDescription.addHistoryItem(factory, trainBaseCd)
                                    }
                                    val newTrainCd = trainBaseCd.addHistoryItem(factory, trainBaseCd)
                                    
                                    (contentDescribableMappedInst, newTrainCd)
                                })
                            )
                            
                            if(verbosity > 2) println("save mappedInstances: " + mappedInst.contentDescription)
                            mappedInst.save()
                            fileHandle.close
                            mappedInst
                        }
                    }
                    case AcceptReceiveFile(file) => {
                        if(verbosity >= 2) println("file exists => load from file (" + targetCd.fullFilename + ")")
                        ArffJsonInstancesSource(targetCd.fullFilename, targetCd)
                    }
                }
            }
            case (trainBase, instBase) => {
                val (mappedInst, filters) = getInstancesAndFilters(trainBase, targetFilters, cat)
                if(trainBase == targetInstBase) mappedInst
                else ((targetInstBase /: filters)((oldInst, filter) => filter.applyFilter(oldInst, cat)))
            }
        }
    }
}















