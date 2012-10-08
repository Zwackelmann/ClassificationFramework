package classifier
import parser.ArffJsonInstancesSource
import parser.ArffJsonInstancesMapping
import parser.ArffJsonInstancesFile
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
import common.ForgetableMap
import scala.collection.mutable.HashMap
import weka.classifiers.trees.J48
import parser.ContentDescription
import format.arff_json.HistoryItem
import format.arff_json.InstancesMappings
import java.io.File

object Learner {
    val serializeClassifiers = true
    
    def resultsFilePath(trainBaseCD: ContentDescription, targetClassDef: TargetClassDefinition, learner: Option[Learner], classifiedInstCd: ContentDescription) = 
        Path.resultsPath / (
            classifierFilename(trainBaseCD, targetClassDef, learner) + "_" +  
            classifiedInstCd.filenameAppendix + 
            ".json"
        )
        
    def classifierFilename(trainBaseCD: ContentDescription, targetClassDef: TargetClassDefinition, parent: Option[Learner]) = trainBaseCD.base + "_" + 
        trainBaseCD.formatHistory.map(_.historyAppendix).mkString("_") + "_" + 
        (parent match {
            case Some(learner) => learner.fileAppendix
            case None => "Independent"
        }) + "_" + 
        targetClassDef.filenameExtension 
    
    def classifierPath(trainBaseCD: ContentDescription, targetClassDef: TargetClassDefinition, parent: Option[Learner]) = 
        Path.classifierPath / classifierFilename(trainBaseCD, targetClassDef, parent)
}

@serializable
trait Learner {
    import RawClassification._
    
    def trainClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): Classifier
    def loadClassifier(file: File): Classifier
    
    def fileAppendix: String
    def targetHistory(targetClassDef: TargetClassDefinition): List[HistoryItem]
    
    def deliverInstances(contentDescription: ContentDescription) = {
        if(contentDescription.file.exists) {
            Some(new ArffJsonInstancesFile(contentDescription))
        } else {
            None
        }
    }
    
    def mapInstances(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, set: Option[ContentDescription.Set]): ArffJsonInstancesSource = {
        val targetContentDescription = {
            val cd1 = inst.contentDescription.withHistory(
                targetHistory(targetClassDef)
            )
            
            set match {
                case Some(set) => cd1.toSet(set)
                case None => cd1
            }
        }
        
        InstancesMappings(
            inst,
            targetContentDescription,
            targetClassDef,
            this
        )
    }
    
    def classifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = {
        val trainBaseCd = ContentDescription(inst.contentDescription.base, ContentDescription.TrainSet, targetHistory(targetClassDef))
        val targetCd = ContentDescription(inst.contentDescription.base, inst.contentDescription.set, targetHistory(targetClassDef))
        val classifierPath = Learner.classifierPath(trainBaseCd, targetClassDef, Some(this))
        
        if(Learner.serializeClassifiers) {
            println("check if " + classifierPath + " exists... ")
        }
        if(Learner.serializeClassifiers && classifierPath.exists()) {
            println("... yes => load classifier from file")
            loadClassifier(classifierPath)
        } else {
            if(Learner.serializeClassifiers) {
                println("... no => train classifier")
            }
            val c = trainClassifier(mapInstances(inst, targetClassDef, Some(ContentDescription.TrainSet)), targetClassDef)
            
            if(Learner.serializeClassifiers) {
                // TODO dirty... make a savable trait or something like that for lerners... or think up something else...
                try {
                    c.save(classifierPath)
                    println("saved " + c + " in file " + classifierPath)
                } catch {
                    case e =>  
                }
            }
            c
        }
    }
    
    def classifications(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = {
        val trainBaseCd = ContentDescription(inst.contentDescription.base, ContentDescription.TrainSet, targetHistory(targetClassDef))
        val targetCd = ContentDescription(inst.contentDescription.base, inst.contentDescription.set, targetHistory(targetClassDef))
        
        if(Classifier.serializeClassifications) {
            val resultsFilePath = Learner.resultsFilePath(trainBaseCd, targetClassDef, Some(this), targetCd)
            println("check if " + resultsFilePath + " exists... ")
            if(resultsFilePath.exists) {
                println("... yes => load results from file")
                RawClassification.fromFile(resultsFilePath)
            } else {
                println("... no => calculate classifications")
                classifier(inst, targetClassDef).classifications(mapInstances(inst, targetClassDef, None))
            }
        } else {
            classifier(inst, targetClassDef).classifications(mapInstances(inst, targetClassDef, None))
        }
    }
    
    def filterAndGroup(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, positiveThreshold: Double = 0.0, certaintyThreshold: Double = 0.0) = {
        Map(
            TRUE_POSITIVE -> List(),
            FALSE_POSITIVE -> List(),
            TRUE_NEGATIVE -> List(),
            FALSE_NEGATIVE -> List()
        ) ++ classifications(inst, targetClassDef).filter(c => math.abs(c.classification - positiveThreshold) > certaintyThreshold).groupBy(_.cat)
    }
    
    def precision(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, certaintyThreshold: Double = 0.0) = {
        val filteredAndGrouped = filterAndGroup(inst, targetClassDef, certaintyThreshold)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_POSITIVE).size)
    }
    
    def recall(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, certaintyThreshold: Double = 0.0) = {
        val filteredAndGrouped = filterAndGroup(inst, targetClassDef, certaintyThreshold)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_NEGATIVE).size)
    }
    
    def fMeasure(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, alpha: Double = 1.0, positiveThreshold: Double = 0.0, certaintyThreshold: Double = 0.0) = {
        val prec = precision(inst, targetClassDef, certaintyThreshold)
        val rec = recall(inst, targetClassDef, certaintyThreshold)
        
        ((1 + alpha) * prec * rec) / ((alpha * prec) + rec)
    }
        
    def report(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, certaintyThreshold: Double = 0.0) {
        val filteredAndGrouped = filterAndGroup(inst, targetClassDef, certaintyThreshold)
        
        println("precision: " + precision(inst, targetClassDef, certaintyThreshold))
        println("recall: " + recall(inst, targetClassDef, certaintyThreshold))
        
        val certainties = classifications(inst, targetClassDef).map(c => math.abs(c.classification))
        println("mean certainty: " + (if(certainties.size != 0) mean(certainties) else "NaN")) 
        println("certainty std dev: " + (if(certainties.size != 0) stdDev(certainties) else "NaN"))
        println("total: " + classifications(inst, targetClassDef).size)
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















