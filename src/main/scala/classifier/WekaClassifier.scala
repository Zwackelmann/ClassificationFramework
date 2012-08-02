package classifier

import new_approach.classifier.Classifier
import new_approach.classifier.ClassifierGenerator
import parser.ArffJsonInstancesSource
import model.RawClassification
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonInstances
import format.arff_json.NominalArffJsonAttribute
import weka.classifiers.{Classifier => WekaInternalClassifier}
import java.io.File
import format.arff_json.ArffJsonHeader

object WekaClassifier {
    def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[WekaClassifier]
}

@serializable
abstract class WekaClassifier(trainBase: ArffJsonInstancesSource, val targetClassDef: TargetClassDefinition, val parent: Option[ClassifierGenerator]) extends Classifier {
    def this(trainBase: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, parent: ClassifierGenerator) = 
        this(trainBase, targetClassDef, Some(parent))
        
    def this(trainBase: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = 
        this(trainBase, targetClassDef, None)
    
    def classifierConfig(): WekaInternalClassifier
    
    val classifier = builtClassifier(trainBase)
    
    def builtClassifier(source: ArffJsonInstancesSource): WekaInternalClassifier = {
        val mappedInstances = mapInstances(source, targetClassDef)
        
        println("built weka classifier with " + mappedInstances.contentDescription)
        
        val instances = {
            val classAttribute = (inst: ArffJsonInstance) => if(targetClassDef(inst.mscClasses)) "yes" else "no"
            new ArffJsonInstances(mappedInstances, List(Pair(new NominalArffJsonAttribute("target_class", List("no", "yes")), classAttribute))).instances
        }
        instances.setClassIndex(0)
        
        val classifier = classifierConfig()
        classifier.buildClassifier(instances)
        
        classifier
    }
    
    def calculateClassifications(inst: ArffJsonInstancesSource) = {
        val mappedInstances = mapInstances(inst, targetClassDef)
        println("calculate classifications for " + mappedInstances.contentDescription)
        
        val arffJsonInstances = new ArffJsonInstances(mappedInstances, List())
        val instances = arffJsonInstances.instances
        val instancesMetadata = arffJsonInstances.instancesMetadata
        
        for (i <- (0 until instances.numInstances()).toIterable) yield {
            val instance = instances.instance(i)
            
            val arffJsonInstance = arffJsonInstances.arffJsonInstance(i)
            val possForYes = classifier.distributionForInstance(instance)(1) // get possibility for "yes"
            
            val trueClass = if(targetClassDef(arffJsonInstance.mscClasses)) 1.0 else -1.0
            new RawClassification(arffJsonInstance.id, (possForYes-0.5)*2, trueClass)
        }
    }
    
    def save(outFile: File) {
        common.ObjectToFile.writeObjectToFile(this, outFile)
    }
}