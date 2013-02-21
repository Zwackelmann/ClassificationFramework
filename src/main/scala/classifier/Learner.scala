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
import common.ForgetableMap
import scala.collection.mutable.HashMap
import weka.classifiers.trees.J48
import parser.ContentDescription
import format.arff_json.InstancesMappings
import java.io.File
import filter.FilterFactory
import parser.ContentDescribable
import filter.CategorySelectionFilter

object Learner {
    val serializeClassifiers = true
    
    def resultsFilename(trainBaseCD: ContentDescription, targetClassDef: TargetClassDefinition, learner: Option[Learner], classifiedInstCd: ContentDescription) =
        classifierFilename(trainBaseCD, targetClassDef, learner) + "_" +  
        classifiedInstCd.filenameAppendix + 
        ".json"
    
    // TODO remove to keep XXXFilename methods consistent
    def resultsFilePath(trainBaseCD: ContentDescription, targetClassDef: TargetClassDefinition, learner: Option[Learner], classifiedInstCd: ContentDescription) = 
        Path.resultsPath / resultsFilename(trainBaseCD, targetClassDef, learner, classifiedInstCd)
        
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
    def targetHistory(targetClassDef: TargetClassDefinition): List[FilterFactory]
    
    def mapInstances(inst: ArffJsonInstancesSource with ContentDescribable, targetClassDef: TargetClassDefinition, set: Option[ContentDescription.Set]): ArffJsonInstancesSource with ContentDescribable = {
        val targetContentDescription = {
            val cd1 = inst.contentDescription.withFilterFactories(
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
    
    
    def classifierIfCached(cd: ContentDescription, targetClassDef: TargetClassDefinition): Option[Classifier] = {
        val trainBaseCd = ContentDescription(cd.base, ContentDescription.TrainSet, targetHistory(targetClassDef))
        val targetCd = ContentDescription(cd.base, cd.set, targetHistory(targetClassDef))
        val classifierPath = Learner.classifierPath(trainBaseCd, targetClassDef, Some(this))
        
        println("check if " + classifierPath + " exists... ")
        if(classifierPath.exists()) {
            println("... yes => load classifier from file")
            try {
            	Some(loadClassifier(classifierPath))
            } catch {
              	case _ => { 
              		val success = classifierPath.delete()
              		println("cannot read classifier file => delete")
              		None
              	}
            }
        } else {
            println("... no => skip")
            None
        }
    }
    
    def classifierIfCached(inst: ArffJsonInstancesSource with ContentDescribable, targetClassDef: TargetClassDefinition): Option[Classifier] = {
        val trainBaseCd = ContentDescription(inst.contentDescription.base, ContentDescription.TrainSet, targetHistory(targetClassDef))
        val targetCd = ContentDescription(inst.contentDescription.base, inst.contentDescription.set, targetHistory(targetClassDef))
        val classifierPath = Learner.classifierPath(trainBaseCd, targetClassDef, Some(this))
        
        println("check if " + classifierPath + " exists... ")
        if(classifierPath.exists()) {
            println("... yes => load classifier from file")
            try {
            	Some(loadClassifier(classifierPath))
            } catch {
              	case _ => { 
              		val success = classifierPath.delete()
              		println("cannot read classifier file => delete")
              		None
              	}
            }
        } else {
            println("... no => skip")
            None
        }
    }
    
    def classifier(inst: ArffJsonInstancesSource with ContentDescribable, targetClassDef: TargetClassDefinition): Classifier = {
        val trainBaseCd = ContentDescription(inst.contentDescription.base, ContentDescription.TrainSet, targetHistory(targetClassDef))
        val targetCd = ContentDescription(inst.contentDescription.base, inst.contentDescription.set, targetHistory(targetClassDef))
        val classifierPath = Learner.classifierPath(trainBaseCd, targetClassDef, Some(this))
        
        if(Learner.serializeClassifiers) {
            println("check if " + classifierPath + " exists... ")
        }
        if(Learner.serializeClassifiers && classifierPath.exists()) {
            println("... yes => load classifier from file")
            try {
            	loadClassifier(classifierPath)
            } catch {
              	case _ => { 
              		val success = classifierPath.delete()
              		println("cannot read classifier file => delete classifier and regenerate")
              		classifier(inst, targetClassDef)
              	}
            }
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
    
    def classificationsIfCached(inst: ArffJsonInstancesSource with ContentDescribable, targetClassDef: TargetClassDefinition, verifyClassifications: Boolean = false): Option[List[RawClassification]] = {
        val trainBaseCd = ContentDescription(inst.contentDescription.base, ContentDescription.TrainSet, targetHistory(targetClassDef))
        val targetCd = ContentDescription(inst.contentDescription.base, inst.contentDescription.set, targetHistory(targetClassDef))
        
        val resultsFilePath = Learner.resultsFilePath(trainBaseCd, targetClassDef, Some(this), targetCd)
        println("check if " + resultsFilePath + " exists... ")
        if(resultsFilePath.exists) {
            println("... yes => load results from file")
            val cachedClassifications = RawClassification.fromFile(resultsFilePath)
            if(verifyClassifications) {
            	println("verify classifications...")
            	val availableIds = cachedClassifications.map(_.id).sortBy(c => c)
            	val necessaryIds = mapInstances(inst, targetClassDef, None).map(_.id).toList.sortBy(c => c)
            	
            	val valid = (availableIds zip necessaryIds).forall(x => x._1 == x._2)
            	if(valid) {
            	    Some(cachedClassifications)
            	} else {
            	    println("verification failed => try to delete results file")
            	    if(!resultsFilePath.delete()) {
            	        throw new RuntimeException("results file is invalid but could not be deleted...");
            	    } else {
            	        println("\n\n\nINVALID RESULTS FILE DELETED!!!\n\n\n")
            	        None
            	    }
            	}
            } else {
                Some(cachedClassifications)
            }
        } else {
            println("... no => skip")
            None
        }
    }
    
    def classificationsIfCached(cd: ContentDescription, targetClassDef: TargetClassDefinition): Option[List[RawClassification]] = {
        val trainBaseCd = ContentDescription(cd.base, ContentDescription.TrainSet, targetHistory(targetClassDef))
        val targetCd = ContentDescription(cd.base, cd.set, targetHistory(targetClassDef))
        
        val resultsFilePath = Learner.resultsFilePath(trainBaseCd, targetClassDef, Some(this), targetCd)
        println("check if " + resultsFilePath + " exists... ")
        if(resultsFilePath.exists) {
            println("... yes => load results from file")
            val cachedClassifications = RawClassification.fromFile(resultsFilePath)
            Some(cachedClassifications)
        } else {
            println("... no => skip")
            None
        }
    }
    
    def classifications(inst: ArffJsonInstancesSource with ContentDescribable, targetClassDef: TargetClassDefinition, verifyClassifications: Boolean = false): List[RawClassification] = {
        val trainBaseCd = ContentDescription(inst.contentDescription.base, ContentDescription.TrainSet, targetHistory(targetClassDef))
        val targetCd = ContentDescription(inst.contentDescription.base, inst.contentDescription.set, targetHistory(targetClassDef))
        
        if(Classifier.serializeClassifications) {
            val resultsFilePath = Learner.resultsFilePath(trainBaseCd, targetClassDef, Some(this), targetCd)
            println("check if " + resultsFilePath + " exists... ")
            if(resultsFilePath.exists) {
                println("... yes => load results from file")
                val cachedClassifications = RawClassification.fromFile(resultsFilePath)
                if(verifyClassifications) {
                	println("verify classifications...")
                	val availableIds = cachedClassifications.map(_.id).toSet
                	val _necessaryIds = negessaryIds(inst, targetClassDef)
                	
                	if(availableIds == _necessaryIds) {
                	    cachedClassifications
                	} else {
                	    println("verification failed => try to delete results file")
                	    if(!resultsFilePath.delete()) {
                	        throw new RuntimeException("delete failed")
                	    } else {
                	        classifications(inst, targetClassDef, verifyClassifications)
                	    }
                	}
                } else {
                    cachedClassifications
                }
            } else {
                println("... no => calculate classifications")
                classifier(inst, targetClassDef).classifications(mapInstances(inst, targetClassDef, None))
            }
        } else {
            classifier(inst, targetClassDef).classifications(mapInstances(inst, targetClassDef, None))
        }
    }
    
    def negessaryIds(inst: ArffJsonInstancesSource, cat: TargetClassDefinition) {
        val filterClasses = targetHistory(cat).filter(_.historyAppendix.substring(0, 3) == "sel").map(_.apply(inst).asInstanceOf[CategorySelectionFilter].targetClass)
        val necessaryids = if(filterClasses.isEmpty) {
            inst.map(_.id).toSet
        } else {
            println("filter categories: " + filterClasses.map(_.filenameExtension))
            inst.filter(i => filterClasses.forall(_.apply(i.categories))).map(_.id)
        }
    }
    
    def filterAndGroup(inst: ArffJsonInstancesSource with ContentDescribable, targetClassDef: TargetClassDefinition, positiveThreshold: Double = 0.0, certaintyThreshold: Double = 0.0) = {
        Map(
            TRUE_POSITIVE -> List(),
            FALSE_POSITIVE -> List(),
            TRUE_NEGATIVE -> List(),
            FALSE_NEGATIVE -> List()
        ) ++ classifications(inst, targetClassDef).filter(c => math.abs(c.classification - positiveThreshold) > certaintyThreshold).groupBy(_.cat)
    }
    
    def precision(inst: ArffJsonInstancesSource with ContentDescribable, targetClassDef: TargetClassDefinition, certaintyThreshold: Double = 0.0) = {
        val filteredAndGrouped = filterAndGroup(inst, targetClassDef, certaintyThreshold)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_POSITIVE).size)
    }
    
    def recall(inst: ArffJsonInstancesSource with ContentDescribable, targetClassDef: TargetClassDefinition, certaintyThreshold: Double = 0.0) = {
        val filteredAndGrouped = filterAndGroup(inst, targetClassDef, certaintyThreshold)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_NEGATIVE).size)
    }
    
    def fMeasure(inst: ArffJsonInstancesSource with ContentDescribable, targetClassDef: TargetClassDefinition, alpha: Double = 1.0, positiveThreshold: Double = 0.0, certaintyThreshold: Double = 0.0) = {
        val prec = precision(inst, targetClassDef, certaintyThreshold)
        val rec = recall(inst, targetClassDef, certaintyThreshold)
        
        ((1 + alpha) * prec * rec) / ((alpha * prec) + rec)
    }
        
    def report(inst: ArffJsonInstancesSource with ContentDescribable, targetClassDef: TargetClassDefinition, certaintyThreshold: Double = 0.0) {
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















