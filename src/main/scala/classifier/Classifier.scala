package classifier
import parser.ArffJsonInstancesSource
import model.RawClassification
import parser.ArffJsonInstancesFile
import common.Path
import common.ForgetableMap
import scala.collection.mutable.HashMap
import format.arff_json.ArffJsonInstance
import parser.ContentDescription
import java.io.File

object Classifier {
    val serializeClassifications = false
    
    import RawClassification._
    
    def filterAndGroup(classifications: Seq[RawClassification], certaintyThreshold: Double = 0.0) = {
        Map(
            TRUE_POSITIVE -> List(),
            FALSE_POSITIVE -> List(),
            TRUE_NEGATIVE -> List(),
            FALSE_NEGATIVE -> List()
        ) ++ classifications.toList.filter(c => math.abs(c.classification) > certaintyThreshold).groupBy(_.cat)
    }
    
    def precision(classifications: Seq[RawClassification], certaintyThreshold: Double = 0.0) = {
        val filteredAndGrouped = filterAndGroup(classifications, certaintyThreshold)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_POSITIVE).size)
    }
    
    def recall(classifications: Seq[RawClassification], certaintyThreshold: Double = 0.0) = {
        val filteredAndGrouped = filterAndGroup(classifications, certaintyThreshold)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_NEGATIVE).size)
    }
    
    def fMeasure(classifications: Seq[RawClassification], alpha: Double, certaintyThreshold: Double = 0.0) = {
        val prec = precision(classifications, certaintyThreshold)
        val rec = recall(classifications, certaintyThreshold)
        
        ((1 + alpha) * prec * rec) / ((alpha * prec) + rec)
    }
        
    def report(classifications: Seq[RawClassification], certaintyThreshold: Double = 0.0) {
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
abstract class Classifier(trainBase: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, parent: Option[Learner] = None) {
    import RawClassification._
    
    def calculateClassifications(inst: ArffJsonInstancesSource): Iterable[RawClassification]
    val filenameAppendix = Learner.classifierPath(trainBase.contentDescription, targetClassDef, parent)
    def save(file: File)
    
    val trainBaseContentDescription = trainBase.contentDescription
    
    def classifications(inst: ArffJsonInstancesSource) = parent match {
        case Some(parent) if Classifier.serializeClassifications => {
            val resultsFilePath = Learner.resultsFilePath(trainBaseContentDescription, targetClassDef, Some(parent), inst.contentDescription)
            
            if(resultsFilePath.exists) {
                RawClassification.fromFile(resultsFilePath)
            } else {
                val results = calculateClassifications(inst)
                RawClassification.save(results, resultsFilePath)
                results.toList
            }
        }
        case None => calculateClassifications(inst).toList
    }
}







