package classifier
import parser.ArffJsonInstancesSource
import model.RawClassification
import parser.ArffJsonInstancesFile
import parser.ContentDescription
import parser.ArffJsonInstancesSource
import common.Path
import java.io.File
import format.arff_json.ArffJsonHeader

class FinalLearner2(learners: List[Learner]) {
    def normalizeClassifications(classifications: Iterable[RawClassification]) = {
        val avgAbsClassification = classifications.map(c => math.abs(c.classification)).reduceLeft(_ + _) / classifications.size
        classifications.map(c => new RawClassification(c.id, c.classification / avgAbsClassification, c.realValue))
    }
    
    def calculateClassifications(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = {
        val resultsAndWeights = learners.map(
            l => {
                val results = normalizeClassifications(l.classifications(inst, targetClassDef))
                val weight = {
                    val m = l.fMeasure(inst, targetClassDef)
                    if(m.isNaN()) 0 else m
                }
                (results, weight) 
            }
        )
        
        val results = resultsAndWeights.map(_._1)
        val weights = resultsAndWeights.map(_._2)
        val normedWeights = weights.map(w => w / weights.reduceLeft(_ + _))
        
        println(results.map(_.size))
        
        val finalRes = results.transpose.map(classificationList => {
            require(classificationList.forall(c => c.id == classificationList(0).id))
            new RawClassification(
                classificationList(0).id, 
                ((0.0 /: (classificationList zip normedWeights))(
                    (old, clw) => old + clw._1.classification * clw._2
                )),
                classificationList(0).realValue)
        })
        
        RawClassification.save(finalRes, new File("data/results/final.json"))
        /*println("weights: " + weights)
        println("rawResults: " + classifiersResults.map(r => r.map(c => c.classification).mkString(" ")).mkString("\n"))
        
        println(classifiersResults.transpose)*/
        
        /*val finalRes = classifiersResults.transpose.map(classificationList => {
            require(classificationList.forall(c => c.id == classificationList(0).id))
            new RawClassification(
                classificationList(0).id, 
                ((0.0 /: (classificationList zip weights))(
                    (old, clw) => old + clw._1.classification * clw._2
                )) / classificationList.size,
                classificationList(0).realValue)
        })
        
        println("finalResults: " + finalRes.take(10).map(c => c.classification).mkString("\n"))*/
    }
}

class FinalLearner(learners: List[Learner]) extends Learner {
    val fileAppendix = "final"
    def targetHistory(targetClassDef: TargetClassDefinition) = List()
    
    def trainClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): Classifier = {
        val f = new FinalClassifier(
            learners.map(
                cg => (
                    cg.classifier(inst, targetClassDef),
                    cg.fMeasure(inst, targetClassDef, 1.0)
                )
            ),
            targetClassDef,
            this
        )
        
        println("final classifier ready")
        f
    }
    
    
    def loadClassifier(file: File) = throw new RuntimeException("A FinalClassifier cannot be saved yet")
}

class FinalClassifier(classifiersWithWeights: List[Pair[Classifier, Double]], val targetClassDef: TargetClassDefinition, val parent: Option[Learner]) extends Classifier(
        ArffJsonInstancesSource(
            List(), 
            new ArffJsonHeader("", List(), List()), 
            ContentDescription("", ContentDescription.TrainSet, List())
        ), 
        targetClassDef, 
        parent
    ) {
    def this(classifiersWithWeights: List[Pair[Classifier, Double]], targetClassDef: TargetClassDefinition, parent: Learner) = 
        this(classifiersWithWeights, targetClassDef, Some(parent))
    
    def this(classifiersWithWeights: List[Pair[Classifier, Double]], targetClassDef: TargetClassDefinition) = 
        this(classifiersWithWeights, targetClassDef, None)
    
    def normalizeClassifications(classifications: Iterable[RawClassification]) = {
        val avgAbsClassification = classifications.map(c => math.abs(c.classification)).reduceLeft(_ + _) / classifications.size
        classifications.map(c => new RawClassification(c.id, c.classification / avgAbsClassification, c.realValue))
    }
    
    def calculateClassifications(inst: ArffJsonInstancesSource) = {
        val classifiers = classifiersWithWeights.map(_._1)
        val weights = classifiersWithWeights.map(cw => if(cw._2.isNaN()) 0 else cw._2)
        val classifiersResults = classifiers.map(c => normalizeClassifications(c.classifications(inst).toList).toList.sortBy(c => c.id)).take(10)
        
        println(classifiersResults)
        
        /*println("weights: " + weights)
        println("rawResults: " + classifiersResults.map(r => r.map(c => c.classification).mkString(" ")).mkString("\n"))
        
        println(classifiersResults.transpose)*/
        
        /*val finalRes = classifiersResults.transpose.map(classificationList => {
            require(classificationList.forall(c => c.id == classificationList(0).id))
            new RawClassification(
                classificationList(0).id, 
                ((0.0 /: (classificationList zip weights))(
                    (old, clw) => old + clw._1.classification * clw._2
                )) / classificationList.size,
                classificationList(0).realValue)
        })
        
        println("finalResults: " + finalRes.take(10).map(c => c.classification).mkString("\n"))*/
        throw new RuntimeException()
    }
    
    def save(file: File) = throw new RuntimeException("A final classifier cannot be saved yet")
}












