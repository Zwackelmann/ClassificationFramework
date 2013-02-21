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
import parser.ContentDescribable

object WekaClassifier {
    def load(file: File, parent: Option[Learner] = None) = {
        val savedObject = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[Array[Any]]
        val wekaClassifier = savedObject(0).asInstanceOf[WekaInternalClassifier]
        val trainBaseCd = savedObject(1).asInstanceOf[Option[ContentDescription]]
        val targetClassDef = savedObject(2).asInstanceOf[TargetClassDefinition]
        
        new WekaClassifier(wekaClassifier, trainBaseCd, targetClassDef, parent)
    }
    
    def apply(trainBase: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, parent: Option[Learner], classifierConfig: () => WekaInternalClassifier) = {
        val classifier = builtClassifier(trainBase, targetClassDef, classifierConfig)
        new WekaClassifier(classifier, trainBase match { case cd: ContentDescribable => Some(cd.contentDescription) case _ => None }, targetClassDef, parent)
    }
    
    def builtClassifier(mappedInst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, classifierConfig: () => WekaInternalClassifier): WekaInternalClassifier = {
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
}

class WekaClassifier(val wekaClassifier: WekaInternalClassifier, val trainBaseCD: Option[ContentDescription], targetClassDef: TargetClassDefinition, parent: Option[Learner]) extends Classifier(trainBaseCD, targetClassDef, parent) {
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
            
            val possForYes = wekaClassifier.distributionForInstance(instance)(1) // get possibility for "yes"
            //val possForYes = classifier.classifyInstance(instance)
            //println(possForYes)
            
            val trueClass = if(targetClassDef(arffJsonInstance.categories)) 1.0 else -1.0
            new RawClassification(arffJsonInstance.id, (possForYes-0.5)*2, trueClass)
        }
    }
    
    def save(outFile: File) {
        val objToSave = Array[Any](
            wekaClassifier,
            trainBaseCD,
            targetClassDef,
            None // parent
        )
        
        common.ObjectToFile.writeObjectToFile(objToSave, outFile)
    }
    
    override def toString = "WekaClassifier(trainBase: " + trainBaseContentDescription + ", targetClassDef: " + targetClassDef + ")"
}















