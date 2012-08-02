package classifier
import parser.ArffJsonInstancesSource
import model.RawClassification
import parser.ArffJsonInstancesFile
import common.Path
import common.ForgetableMap
import scala.collection.mutable.HashMap
import format.arff_json.ArffJsonInstance

object Classifier {
    import RawClassification._
    
    def filterAndGroup(classifications: Seq[RawClassification], positiveThreshold: Double = 0.0, certaintyThreshold: Double = 0.0) = {
        Map(
            TRUE_POSITIVE -> List(),
            FALSE_POSITIVE -> List(),
            TRUE_NEGATIVE -> List(),
            FALSE_NEGATIVE -> List()
        ) ++ classifications.toList.filter(c => math.abs(c.classification - positiveThreshold) > certaintyThreshold).groupBy(_.cat(positiveThreshold))
    }
    
    def precision(classifications: Seq[RawClassification], positiveThreshold: Double = 0.0, certaintyThreshold: Double = 0.0) = {
        val filteredAndGrouped = filterAndGroup(classifications, positiveThreshold, certaintyThreshold)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_POSITIVE).size)
    }
    
    def recall(classifications: Seq[RawClassification], positiveThreshold: Double = 0.0, certaintyThreshold: Double = 0.0) = {
        val filteredAndGrouped = filterAndGroup(classifications, positiveThreshold, certaintyThreshold)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_NEGATIVE).size)
    }
        
    def report(classifications: Seq[RawClassification], positiveThreshold: Double = 0.0, certaintyThreshold: Double = 0.0) {
        println("precision: %.5f".format(precision(classifications, positiveThreshold, certaintyThreshold)))
        println("recall: %.5f".format(recall(classifications, positiveThreshold, certaintyThreshold)))
        
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

trait Classifier {
    import RawClassification._
    
    def targetClassDef: TargetClassDefinition
    def calculateClassifications(inst: ArffJsonInstancesSource): Iterable[RawClassification]
    def parent: Option[ClassifierGenerator]
    
    def mapInstances(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = parent match {
        case Some(parent) => parent.mapInstances(inst, targetClassDef)
        case None => inst
    }
    
    def classifications(inst: ArffJsonInstancesSource) = parent match {
        case Some(parent) => parent.classificationsCache.getOrElseUpdate((inst, targetClassDef), {
            val resultsFilePath = ClassifierGenerator.resultsFilePath(parent, targetClassDef, inst)
            
            if(resultsFilePath.exists) {
                RawClassification.fromFile(resultsFilePath)
            } else {
                val results = calculateClassifications(inst)
                RawClassification.save(results, resultsFilePath)
                results.toList
            }
        })
        case None => calculateClassifications(inst).toList
    }
}