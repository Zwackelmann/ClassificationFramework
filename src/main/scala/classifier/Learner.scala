package classifier
import parser.ArffJsonInstancesSource
import filter.VectorFromDictFilter
import common.Path
import common.Path._
import filter.TfIdfFilter
import filter.NormalizeVectorFilter
import external.GensimLsiFilter
import external.JoachimsSVMLearnApplier
import external.JoachimsSVMClassifier
import model.RawClassification
import weka.classifiers.bayes.NaiveBayes
import scala.collection.mutable.HashMap
import weka.classifiers.trees.J48
import parser.ContentDescription
import format.arff_json.InstancesMappings
import java.io.File
import filter.FilterFactory
import parser.ContentDescribable
import filter.CategorySelectionFilter
import filter.Filter
import common.FileManager
import FileManager.Protocol._

object Learner {
    val serializeClassifiers = true
    
    def classificationsFilename(trainBaseCD: ContentDescription, cat: CategoryIs, learner: Option[Learner], targetInstCd: ContentDescription) = 
        (classifierFilename(trainBaseCD, cat, learner) + "_" +  
        targetInstCd.filename) + 
        ".json"
    
    def classificationsFullFilename(trainBaseCD: ContentDescription, categoryIs: CategoryIs, learner: Option[Learner], classifiedInstCd: ContentDescription) = 
        Path.resultsPath / classificationsFilename(trainBaseCD, categoryIs, learner, classifiedInstCd)
        
    def classifierFilename(trainBaseCD: ContentDescription, cat: CategoryIs, parent: Option[Learner]) = trainBaseCD.base + "_" + 
        trainBaseCD.filename + "_" + 
        (parent match {
            case Some(learner) => learner.fileAppendix
            case None => "independent"
        }) + "_" + 
        cat.filenameExtension
    
    def classifierFullFilename(trainBaseCD: ContentDescription, categoryIs: CategoryIs, parent: Option[Learner]) = 
        Path.classifierPath / classifierFilename(trainBaseCD, categoryIs, parent)
}

trait Learner extends Serializable {
    import RawClassification._
    import common.Common.verbosity
    
    def trainClassifier(inst: ArffJsonInstancesSource, categoryIs: CategoryIs): Classifier
    def loadClassifier(fullFilename: String): Classifier  // TODO gehoert hier eigentlich nicht hin... eigentlich zum Classifier Companion
    
    def fileAppendix: String
    def targetHistory(categoryIs: CategoryIs): List[FilterFactory]
    
    def mapInstances(trainBase: ArffJsonInstancesSource, targetInstBase: ArffJsonInstancesSource, cat: CategoryIs): ArffJsonInstancesSource = {
        val targetFilters = targetHistory(cat)
        
        InstancesMappings.getInstances(
            trainBase,
            targetFilters,
            targetInstBase,
            cat
        )
    }
    
    def filters(trainBase: ArffJsonInstancesSource, cat: CategoryIs) = {
        val (_, filters) = InstancesMappings.getInstancesAndFilters(trainBase, targetHistory(cat), cat)
        filters
    }
    
    def instancesMapperLike(trainBase: ArffJsonInstancesSource, cat: CategoryIs) = {
        val f = filters(trainBase, cat)
        (inst: ArffJsonInstancesSource) => {
            ((inst /: f)((oldInst, filter) => filter.applyFilter(oldInst, cat)))
        }
    }
    
    def classifier(trainBase: ArffJsonInstancesSource, cat: CategoryIs): Classifier = {
        trainBase match {
            case contentDescribableTrainBase: ContentDescribable => {
                val trainBaseCd = InstancesMappings.foldHistoryIntoCd(contentDescribableTrainBase.contentDescription, targetHistory(cat))
                val classifierFullFilename = Learner.classifierFullFilename(trainBaseCd, cat, Some(this))
                if(verbosity >= 2) println("check if " + classifierFullFilename + " exists... ")
                
                (FileManager !? CreateOrReceiveFile(classifierFullFilename)) match {
                    case AcceptReceiveFile(file) => {
                        if(verbosity >= 2) println("... yes => load classifier from file")
                        try {
                            loadClassifier(classifierFullFilename)
                        } catch {
                            case ex: Throwable => {
                                println(ex.getMessage())
                                val success = file.delete()
                                if(verbosity >= 1) println("cannot read classifier file => delete classifier and regenerate")
                                classifier(trainBase, cat)
                            }
                        }
                    }
                    case AcceptCreateFile(fileHandle) => {
                        if(verbosity >= 2) println("... no => train classifier")
                        val c = trainClassifier(mapInstances(trainBase, trainBase, cat), cat)
                        
                        if(Learner.serializeClassifiers) {
                            // TODO dirty... make a saveable trait or something like that for lerners... or think up something else...
                            try {
                                c.save(classifierFullFilename)
                                if(verbosity >= 2) println("saved " + c + " in file " + classifierFullFilename)
                            } catch {
                                case e: Throwable => println("WARNING: the serialization of classifier " + classifierFullFilename + " failed")
                            }
                        }
                        c
                    }
                }
            }
            case inst => {
                trainClassifier(mapInstances(trainBase, trainBase, cat), cat)
            }
        }
    }
    
    def classifications(trainInst: ArffJsonInstancesSource, targetInst: ArffJsonInstancesSource, cat: CategoryIs): List[RawClassification] = {
        (trainInst, targetInst) match {
            case (describableTrainInst: ContentDescribable, describableTargetInst: ContentDescribable) => {
                val classificationsFullFilename = Learner.classificationsFullFilename(
                    InstancesMappings.foldHistoryIntoCd(describableTrainInst.contentDescription, targetHistory(cat)), 
                    cat, 
                    Some(this), 
                    InstancesMappings.foldHistoryIntoCd(describableTargetInst.contentDescription, targetHistory(cat))
                )
                
                if(verbosity >= 2) println("check if " + classificationsFullFilename + " exists")
                
                (FileManager !? CreateOrReceiveFile(classificationsFullFilename)) match {
                    case AcceptReceiveFile(file) => {
                        if(verbosity >= 2) println("yes... load classifications from file")
                        RawClassification.fromFile(classificationsFullFilename)
                    }
                    case AcceptCreateFile(fileHandle) => {
                        if(verbosity >= 2) println("... no => calculate classifications")
                        val cl = classifier(trainInst, cat).classifications(mapInstances(trainInst, targetInst, cat))
                        RawClassification.save(cl, classificationsFullFilename)
                        cl
                    }
                    case Error(msg) => throw new RuntimeException(msg)
                }
            }
            case (trainInst, targetInst) => {
                if(verbosity >= 2) println("trainBase of targetInst not ContentDescribable => calculate classifications from scratch")
                classifier(trainInst, cat).classifications(mapInstances(trainInst, targetInst, cat))
            }
        }
    }
    
    /*def negessaryIds(inst: ArffJsonInstancesSource, categoryIs: CategoryIs) {
        val filterClasses = targetHistory(categoryIs).filter(_.historyAppendix.substring(0, 3) == "sel").map(_.apply(inst).asInstanceOf[CategorySelectionFilter].categoryIs)
        val necessaryids = if(filterClasses.isEmpty) {
            inst.map(_.id).toSet
        } else {
            if(verbosity >= 1) println("filter categories: " + filterClasses.map(_.filenameExtension))
            inst.filter(i => filterClasses.forall(_.apply(i.categories))).map(_.id)
        }
    }*/
    
    def filterAndGroup(trainInst: ArffJsonInstancesSource, targetInst: ArffJsonInstancesSource, cat: CategoryIs, positiveThreshold: Double = 0.0) = {
        Map(
            TRUE_POSITIVE -> List(),
            FALSE_POSITIVE -> List(),
            TRUE_NEGATIVE -> List(),
            FALSE_NEGATIVE -> List()
        ) ++ classifications(trainInst, targetInst, cat).filter(c => math.abs(c.classification - positiveThreshold) > 0.0).groupBy(_.cat)
    }
    
    def precision(trainInst: ArffJsonInstancesSource, targetInst: ArffJsonInstancesSource, cat: CategoryIs, positiveThreshold: Double = 0.0) = {
        val filteredAndGrouped = filterAndGroup(trainInst, targetInst, cat, positiveThreshold)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_POSITIVE).size)
    }
    
    def recall(trainInst: ArffJsonInstancesSource, targetInst: ArffJsonInstancesSource, cat: CategoryIs, positiveThreshold: Double = 0.0) = {
        val filteredAndGrouped = filterAndGroup(trainInst, targetInst, cat, positiveThreshold)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_NEGATIVE).size)
    }
    
    def fMeasure(trainInst: ArffJsonInstancesSource, targetInst: ArffJsonInstancesSource, cat: CategoryIs, alpha: Double = 1.0, positiveThreshold: Double = 0.0) = {
        val prec = precision(trainInst, targetInst, cat, positiveThreshold)
        val rec = recall(trainInst, targetInst, cat, positiveThreshold)
        
        ((1 + alpha) * prec * rec) / ((alpha * prec) + rec)
    }
    
    def mean(values: Iterable[Double]) = {
        values.reduceLeft(_ + _) / values.size
    }
    
    def variance(values: Iterable[Double]) = {
        val m = mean(values)
        values.map(v => (m - v) * (m - v)).reduceLeft(_ + _) / values.size
    }
    
    def stdDev(values: Iterable[Double]) = math.sqrt(variance(values))
}















