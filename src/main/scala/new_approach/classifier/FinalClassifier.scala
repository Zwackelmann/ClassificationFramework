package new_approach.classifier
import parser.ArffJsonInstancesSource
import classifier.TargetClassDefinition
import model.RawClassification
import parser.ArffJsonInstancesFile
import classifier.TopClassIs
import parser.ContentDescription
import parser.ArffJsonInstancesSource
import common.Path
import parser.InstancesMappings

object FinalClassifier {
    def main(args: Array[String]) {
        val finalGen = new FinalClassifierGenerator(
            List(
                AbstractOnlyOddsRatioC45Generator/*,
                TitleOnlyOddsRatioSVMGenerator,
                AbstractOnlyLsiSVMGenerator,
                AbstractOnlyOddsRatioSVMGenerator,
                JournalOnlyBayesGenerator,
                TermsOnlyBayesGenerator*/
            )
        )
        
        val testset = new ArffJsonInstancesFile("final", "test", List())
        
        for(topClass <- common.Common.topClasses) {
            println("\n\nStart evaluating results for topClass: " + topClass)
            finalGen.classifications(testset, TopClassIs(topClass))
        }
    }
}

class FinalClassifierGenerator(classifierGenerators: List[ClassifierGenerator]) extends ClassifierGenerator {
    val fileAppendix = "final"
    def mapInstances(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = inst
    
    def generateClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): Classifier = {
        val weights = classifierGenerators.map(cg => (cg.precision(inst, targetClassDef) + cg.recall(inst, targetClassDef, 0)) / 2)
        new FinalClassifier(
            classifierGenerators.map(
                cg => (
                    cg.classifier(inst, targetClassDef), 
                    (cg.precision(inst, targetClassDef) + cg.recall(inst, targetClassDef, 0)) / 2
                )
            ), 
            targetClassDef, 
            this
        )
    }
}

class FinalClassifier(classifiersWithWeights: List[Pair[Classifier, Double]], val targetClassDef: TargetClassDefinition, val parent: Option[ClassifierGenerator]) extends Classifier {
    def this(classifiersWithWeights: List[Pair[Classifier, Double]], targetClassDef: TargetClassDefinition, parent: ClassifierGenerator) = 
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
        val classifiersResults = classifiers.map(c => normalizeClassifications(c.classifications(inst).toList).toList.sortBy(c => c.id))
        
        classifiersResults.transpose.map(classificationList => {
            require(classificationList.forall(c => c.id == classificationList(0).id))
            new RawClassification(
                classificationList(0).id, 
                ((0.0 /: (classificationList zip weights))(
                    (old, clw) => old + clw._1.classification * clw._2
                )) / classificationList.size, 
                classificationList(0).realValue)
        })
    }
}












