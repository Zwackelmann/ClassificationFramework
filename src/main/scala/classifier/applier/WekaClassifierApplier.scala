package classifier.applier

import java.io.File
import weka.core.Instances
import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.FileReader
import java.io.FileWriter
import common.ObjectToFile.readObjectFromFile
import weka.classifiers.{Classifier => WekaClassifier}
import model.Classification
import model.RawClassification
import common.Common.getFileExtension
import format.arff_json.ArffJsonInstances
import parser.ArffJsonInstancesSource
import classifier.TargetClassDefinition
import new_approach.classifier.Classifier

/*class WekaClassifierApplier(val classifierFile: File) extends Classifier {
    val classifier = readObjectFromFile(classifierFile).asInstanceOf[WekaClassifier]
    
    def id = classifierFile.getAbsolutePath()
    
    def calculateClassifications(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = {
        val arffJsonInstances = new ArffJsonInstances(inst, List())
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
    
    def applyClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, resultsFile: File) {
        val classificationIterator = classifications(inst, targetClassDef)
        
        val resultWriter = new BufferedWriter(new FileWriter(resultsFile))
        for (cl <- classificationIterator) {
            resultWriter.write(cl.toString + "\n")
        }
        
        resultWriter.close()
    }
    
    def applyClassifier(instancesFile: File, intsanceIdsFile: File, resultsFile: File) {
        if(getFileExtension(instancesFile.getName()) == "json") {
            throw new RuntimeException("Use method applyClassifier(File, File) for json files - leave out the instanceIdsFile")
        }
        if(getFileExtension(instancesFile.getName()) != "arff") {
            throw new RuntimeException("Unknown file extension")
        }
        
        val reader = new BufferedReader(new FileReader(instancesFile))
        val instances = new Instances(reader)
        instances.setClassIndex(0)
        
        val instancesIdReader = new BufferedReader(new FileReader(intsanceIdsFile))
        val resultWriter = new BufferedWriter(new FileWriter(resultsFile))
        
        for (i <- 0 until instances.numInstances()) {
            val instance = instances.instance(i)
            
            val possForYes = classifier.distributionForInstance(instance)(1) // get possibility for "yes"
            resultWriter.write(Classification(instancesIdReader.readLine(), possForYes, instance.value(0)).toString + "\n")
        }
        
        resultWriter.close()
    }
}*/