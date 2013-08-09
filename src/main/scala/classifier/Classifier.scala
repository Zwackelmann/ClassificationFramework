package classifier
import parser.ArffJsonInstancesSource
import model.RawClassification
import common.Path
import scala.collection.mutable.HashMap
import format.arff_json.ArffJsonInstance
import parser.ContentDescription
import java.io.File
import parser.ContentDescribable
import common.FileManager
import FileManager.Protocol._

object Classifier {
    val serializeClassifications = true
    
    import RawClassification._
    
    def performanceValues(classifications: Iterable[RawClassification]) = {
        Map(
            TRUE_POSITIVE -> List(),
            FALSE_POSITIVE -> List(),
            TRUE_NEGATIVE -> List(),
            FALSE_NEGATIVE -> List()
        ) ++ classifications.toList.groupBy(_.cat)
    }
    
    def precision(classifications: Iterable[RawClassification]) = {
        val filteredAndGrouped = performanceValues(classifications)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_POSITIVE).size)
    }
    
    def recall(classifications: Iterable[RawClassification]) = {
        val filteredAndGrouped = performanceValues(classifications)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_NEGATIVE).size)
    }
    
    def fMeasure(classifications: Iterable[RawClassification], alpha: Double) = {
        val prec = precision(classifications)
        val rec = recall(classifications)
        
        ((1 + alpha) * prec * rec) / ((alpha * prec) + rec)
    }
        
    def report(classifications: Iterable[RawClassification]) {
        println("precision: %.5f".format(precision(classifications)))
        println("recall: %.5f".format(recall(classifications)))
        
        val certainties = classifications.map(c => math.abs(c.classification))
        // println("mean certainty: " + (if(certainties.size != 0) mean(certainties) else "NaN")) 
        // println("certainty std dev: " + (if(certainties.size != 0) stdDev(certainties) else "NaN"))
        // println("total: " + classifications.size)
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

@serializable
abstract class Classifier(val trainBaseContentDescription: Option[ContentDescription], val targetClassDef: CategoryIs, val parent: Option[Learner]) {
    import RawClassification._
    
    def this(trainBase: ArffJsonInstancesSource, categoryIs: CategoryIs, parent: Option[Learner]) = this(
        (trainBase match {
            case co: ContentDescribable => Some(co.contentDescription)
            case _ => None
        }),
        categoryIs,
        parent
    )
    
    def calculateClassifications(inst: ArffJsonInstancesSource): Iterable[RawClassification]
    def save(fullFilename: String)
    
    // def filenameAppendix = trainBaseContentDescription.map(cd => Learner.classifierFilename(cd, targetClassDef, parent))
    
    def classifications(inst: ArffJsonInstancesSource) = parent match {
        case Some(parent) if Classifier.serializeClassifications => {
            val classificationsFullFilename = for(
                trainCd <- trainBaseContentDescription;
                instCd <- (inst match {
                    case co: ContentDescribable => Some(co.contentDescription)
                    case _ => None
                })
            ) yield Learner.classificationsFullFilename(trainCd, targetClassDef, Some(parent), instCd)
            
            if(classificationsFullFilename.isDefined) {
                (FileManager !? CreateOrReceiveFile(classificationsFullFilename.get)) match {
                    case AcceptReceiveFile(file) => {
                        RawClassification.fromFile(classificationsFullFilename.get)
                    }
                    case AcceptCreateFile(fileHandle) => {
                        val results = calculateClassifications(inst)
                        RawClassification.save(results, classificationsFullFilename.get)
                        results.toList
                    }
                }
            } else {
                calculateClassifications(inst).toList
            }
        }
        case None => calculateClassifications(inst).toList
    }
}







