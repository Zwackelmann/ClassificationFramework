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

object ClassifierGenerator {
    def resultsFilePath(classifierGen: ClassifierGenerator, targetClassDef: TargetClassDefinition, inst: ArffJsonInstancesSource) = 
        Path.resultsPath / (classifierGen.fileAppendix + "_" + targetClassDef.filenameExtension + "_" + inst.contentDescription.filenameAppendix + ".json")
}

@serializable
trait ClassifierGenerator {
    import RawClassification._
    
    def generateClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): Classifier
    
    def fileAppendix: String
    def mapInstances(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): ArffJsonInstancesSource
    
    val classifierCache = new HashMap[Pair[ArffJsonInstancesSource, TargetClassDefinition], Classifier]
    def classifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = classifierCache.getOrElseUpdate((inst, targetClassDef), {
        generateClassifier(inst, targetClassDef)
    })
    
    val classificationsCache = new HashMap[Pair[ArffJsonInstancesSource, TargetClassDefinition], List[RawClassification]]
    def classifications(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = classificationsCache.getOrElseUpdate((inst, targetClassDef), {
        val resultsFilePath = ClassifierGenerator.resultsFilePath(this, targetClassDef, inst)
        if(resultsFilePath.exists) RawClassification.fromFile(resultsFilePath)
        else classifier(inst, targetClassDef).classifications(inst)
    })
    
    val filterAndGroupCache = new HashMap[Tuple4[ArffJsonInstancesSource, TargetClassDefinition, Double, Double], Map[RawClassification.Category, Iterable[RawClassification]]]
    def filterAndGroup(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, positiveThreshold: Double = 0.0, certaintyThreshold: Double = 0.0) = filterAndGroupCache.getOrElseUpdate((inst, targetClassDef, positiveThreshold, certaintyThreshold), {
        Map(
            TRUE_POSITIVE -> List(),
            FALSE_POSITIVE -> List(),
            TRUE_NEGATIVE -> List(),
            FALSE_NEGATIVE -> List()
        ) ++ classifications(inst, targetClassDef).filter(c => math.abs(c.classification - positiveThreshold) > certaintyThreshold).groupBy(_.cat(positiveThreshold))
    })
    
    def precision(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, positiveThreshold: Double = 0.0, certaintyThreshold: Double = 0.0) = {
        val filteredAndGrouped = filterAndGroup(inst, targetClassDef, positiveThreshold, certaintyThreshold)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_POSITIVE).size)
    }
    
    def recall(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, positiveThreshold: Double = 0.0, certaintyThreshold: Double = 0.0) = {
        val filteredAndGrouped = filterAndGroup(inst, targetClassDef, positiveThreshold, certaintyThreshold)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_NEGATIVE).size)
    }
        
    def report(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, positiveThreshold: Double = 0.0, certaintyThreshold: Double = 0.0) {
        val filteredAndGrouped = filterAndGroup(inst, targetClassDef, positiveThreshold, certaintyThreshold)
        
        println("precision: " + precision(inst, targetClassDef, positiveThreshold, certaintyThreshold))
        println("recall: " + recall(inst, targetClassDef, positiveThreshold, certaintyThreshold))
        
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















