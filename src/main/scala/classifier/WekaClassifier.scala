package classifier

import parser.ArffJsonInstancesSource
import model.RawClassification
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffInstances
import format.arff_json.NominalArffJsonAttribute
import weka.classifiers.{Classifier => WekaInternalClassifier}
import java.io.File
import format.arff_json.ArffJsonHeader
import parser.ContentDescription
import parser.ContentDescribable
import common.FileManager
import FileManager.Protocol._

object WekaClassifier {
    def load(fullFilename: String, parent: Option[Learner] = None) = {
        val savedObject = (FileManager !? ReceiveFile(fullFilename)) match {
            case AcceptReceiveFile(file) => {
                common.ObjectToFile.readObjectFromFile(file).asInstanceOf[Array[Any]]
            }
            case FileNotExists => throw new RuntimeException(fullFilename + " does not exist")
        }
        
        val wekaClassifier = savedObject(0).asInstanceOf[WekaInternalClassifier]
        val trainBaseCd = savedObject(1).asInstanceOf[Option[ContentDescription]]
        val targetClassDef = savedObject(2).asInstanceOf[CategoryIs]
        
        new WekaClassifier(wekaClassifier, trainBaseCd, targetClassDef, parent)
    }
    
    def apply(trainBase: ArffJsonInstancesSource, categoryIs: CategoryIs, parent: Option[Learner], classifierConfig: () => WekaInternalClassifier) = {
        val classifier = builtClassifier(trainBase, categoryIs, classifierConfig)
        new WekaClassifier(classifier, trainBase match { case cd: ContentDescribable => Some(cd.contentDescription) case _ => None }, categoryIs, parent)
    }
    
    def builtClassifier(mappedInst: ArffJsonInstancesSource, categoryIs: CategoryIs, classifierConfig: () => WekaInternalClassifier): WekaInternalClassifier = {
        val instances = {
            val classAttribute = (inst: ArffJsonInstance) => {
                val isTarget = categoryIs.matchesForTraining(inst.categories) 
                if(isTarget.isDefined && isTarget.get) "yes" else if(isTarget.isDefined) "no" else "?"
            }
            val arffJsonInst = new ArffInstances(mappedInst, List(Pair(new NominalArffJsonAttribute("target_class", List("no", "yes", "?")), classAttribute)))
            arffJsonInst.instances
        }
        
        instances.setClassIndex(0)
        
        val classifier = classifierConfig()
        classifier.buildClassifier(instances)
        
        classifier
    }
}

class WekaClassifier(val wekaClassifier: WekaInternalClassifier, val trainBaseCD: Option[ContentDescription], categoryIs: CategoryIs, parent: Option[Learner]) extends Classifier(trainBaseCD, categoryIs, parent) {
    def calculateClassifications(mappedInst: ArffJsonInstancesSource) = {
        val arffJsonInstances = {
            val classAttribute = (inst: ArffJsonInstance) => {
                val isTarget = categoryIs.matchesForTesting(inst.categories) 
                if(isTarget.isDefined && isTarget.get) "yes" else if(isTarget.isDefined) "no" else "?"
            }
            val arffJsonInstances = new ArffInstances(mappedInst, List(Pair(new NominalArffJsonAttribute("target_class", List("no", "yes")), classAttribute)))
            arffJsonInstances.instances.setClassIndex(0)
            arffJsonInstances
        }
        
        for (i <- (0 until arffJsonInstances.numInstances).toIterable) yield {
            val instance = arffJsonInstances.instances.instance(i)
            val arffJsonInstance = arffJsonInstances.arffJsonInstance(i)
            
            val possForYes = wekaClassifier.distributionForInstance(instance)(1) // get possibility for "yes"
            //val possForYes = classifier.classifyInstance(instance)
            //println(possForYes)
            
            val trueClass = {
                val isTarget = targetClassDef.matchesForTesting(arffJsonInstance.categories)
                if(isTarget.isDefined && isTarget.get) 1.0 else if(isTarget.isDefined) -1.0 else 0.0
            }
            
            new RawClassification(arffJsonInstance.id, (possForYes-0.5)*2, trueClass)
        }
    }
    
    def save(fullFilename: String) {
        val objToSave = Array[Any](
            wekaClassifier,
            trainBaseCD,
            targetClassDef,
            None // parent
        )
        
        (FileManager !? CreateFile(fullFilename)) match {
            case AcceptCreateFile(fileHandle) => 
                common.ObjectToFile.writeObjectToFile(objToSave, fileHandle.file)
                fileHandle.close
            case RejectCreateFile => throw new RuntimeException("Could not save Weka Classifier")
        }
    }
    
    override def toString = "WekaClassifier(trainBase: " + trainBaseContentDescription + ", targetClassDef: " + targetClassDef + ")"
}















