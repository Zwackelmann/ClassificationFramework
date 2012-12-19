package classifier
import parser.ArffJsonInstancesSource
import model.RawClassification
import common.Path
import common.ForgetableMap
import scala.collection.mutable.HashMap
import format.arff_json.ArffJsonInstance
import parser.ContentDescription
import java.io.File
import parser.ContentDescribable

object Classifier {
    val serializeClassifications = true
    
    import RawClassification._
    
    def performanceValues(classifications: Iterable[RawClassification], certaintyThreshold: Double = 0.0) = {
        Map(
            TRUE_POSITIVE -> List(),
            FALSE_POSITIVE -> List(),
            TRUE_NEGATIVE -> List(),
            FALSE_NEGATIVE -> List()
        ) ++ classifications.toList.filter(c => math.abs(c.classification) > certaintyThreshold).groupBy(_.cat)
    }
    
    def precision(classifications: Iterable[RawClassification], certaintyThreshold: Double = 0.0) = {
        val filteredAndGrouped = performanceValues(classifications, certaintyThreshold)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_POSITIVE).size)
    }
    
    def recall(classifications: Iterable[RawClassification], certaintyThreshold: Double = 0.0) = {
        val filteredAndGrouped = performanceValues(classifications, certaintyThreshold)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_NEGATIVE).size)
    }
    
    def fMeasure(classifications: Iterable[RawClassification], alpha: Double, certaintyThreshold: Double = 0.0) = {
        val prec = precision(classifications, certaintyThreshold)
        val rec = recall(classifications, certaintyThreshold)
        
        ((1 + alpha) * prec * rec) / ((alpha * prec) + rec)
    }
        
    def report(classifications: Iterable[RawClassification], certaintyThreshold: Double = 0.0) {
        println("precision: %.5f".format(precision(classifications, certaintyThreshold)))
        println("recall: %.5f".format(recall(classifications, certaintyThreshold)))
        
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
abstract class Classifier(val trainBaseContentDescription: Option[ContentDescription], targetClassDef: TargetClassDefinition, parent: Option[Learner]) {
    import RawClassification._
    
    def this(trainBase: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, parent: Option[Learner]) = this(
        (trainBase match {
            case co: ContentDescribable => Some(co.contentDescription)
            case _ => None
        }),
        targetClassDef,
        parent
    )
    
    def calculateClassifications(inst: ArffJsonInstancesSource): Iterable[RawClassification]
    def save(file: File)
    
    def filenameAppendix = trainBaseContentDescription.map(cd => Learner.classifierPath(cd, targetClassDef, parent))
    
    def classifications(inst: ArffJsonInstancesSource) = parent match {
        case Some(parent) if Classifier.serializeClassifications => {
            val resultsFilePath = for(
                trainCd <- trainBaseContentDescription;
                instCd <- (inst match {
                    case co: ContentDescribable => Some(co.contentDescription)
                    case _ => None
                })
            ) yield Learner.resultsFilePath(trainCd, targetClassDef, Some(parent), instCd)
            
            if(resultsFilePath.isDefined && resultsFilePath.get.exists) {
                RawClassification.fromFile(resultsFilePath.get)
            } else {
                val results = calculateClassifications(inst)
                
                if(resultsFilePath.isDefined) {
                    RawClassification.save(results, resultsFilePath.get)
                }
                results.toList
            }
        }
        case None => calculateClassifications(inst).toList
    }
}







