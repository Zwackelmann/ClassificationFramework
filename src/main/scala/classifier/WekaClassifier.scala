package classifier

import parser.ArffJsonInstancesSource
import model.RawClassification
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonInstances
import format.arff_json.NominalArffJsonAttribute
import weka.classifiers.{Classifier => WekaInternalClassifier}
import java.io.File
import format.arff_json.ArffJsonHeader
import parser.ContentDescription

object WekaClassifier {
    def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[WekaClassifier]
}

@serializable
abstract class WekaClassifier(@transient trainBase: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, parent: Option[Learner]) extends Classifier(trainBase, targetClassDef, parent) {
    def this(trainBase: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, parent: Learner) = 
        this(trainBase, targetClassDef, Some(parent))
        
    def this(trainBase: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = 
        this(trainBase, targetClassDef, None)
    
    def classifierConfig(): WekaInternalClassifier
    
    val classifier = builtClassifier(trainBase)
    
    def builtClassifier(mappedInst: ArffJsonInstancesSource): WekaInternalClassifier = {
        val instances = {
            val classAttribute = (inst: ArffJsonInstance) => if(targetClassDef(inst.categories)) "yes" else "no"
            val arffJsonInst = new ArffJsonInstances(mappedInst, List(Pair(new NominalArffJsonAttribute("target_class", List("no", "yes")), classAttribute)))
            arffJsonInst.instances
        }
        
        instances.setClassIndex(0)
        
        val classifier = classifierConfig()
        classifier.buildClassifier(instances)
        
        classifier
    }
    
    def calculateClassifications(mappedInst: ArffJsonInstancesSource) = {
        val arffJsonInstances = {
            val classAttribute = (inst: ArffJsonInstance) => if(targetClassDef(inst.categories)) "yes" else "no"
            val arffJsonInstances = new ArffJsonInstances(mappedInst, List(Pair(new NominalArffJsonAttribute("target_class", List("no", "yes")), classAttribute)))
            arffJsonInstances.instances.setClassIndex(0)
            arffJsonInstances
        }
        
        for (i <- (0 until arffJsonInstances.numInstances).toIterable) yield {
            val instance = arffJsonInstances.instances.instance(i)
            val arffJsonInstance = arffJsonInstances.arffJsonInstance(i)
            
            val possForYes = classifier.distributionForInstance(instance)(1) // get possibility for "yes"
            //val possForYes = classifier.classifyInstance(instance)
            //println(possForYes)
            
            val trueClass = if(targetClassDef(arffJsonInstance.categories)) 1.0 else -1.0
            new RawClassification(arffJsonInstance.id, (possForYes-0.5)*2, trueClass)
        }
    }
    
    def save(outFile: File) {
        common.ObjectToFile.writeObjectToFile(this, outFile)
    }
    
    override def toString = "WekaClassifier(trainBase: " + trainBaseContentDescription + ", targetClassDef: " + targetClassDef + ")"
}