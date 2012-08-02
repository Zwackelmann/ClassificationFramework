package classifier.generator

import weka.classifiers.Classifier
import java.io.File
import weka.core.Instances
import java.io.BufferedReader
import java.io.FileReader
import common.ObjectToFile.{writeObjectToFile => saveClassifier}
import common.Common.getFileExtension
import format.arff_json.ArffJsonInstances
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonAttribute
import format.arff_json.NominalArffJsonAttribute
import parser.ArffJsonInstancesFile
import parser.ArffJsonInstancesSource

object WekaClassifierGenerator {
    def saveClassifier(classifier: Classifier, file: File) {
        saveClassifier(classifier, file)
    }
}

abstract class WekaClassifierGenerator {
    def classifierConfig(): Classifier
    
    def genClassifier(instancesFile: File, outputFile: File) {
        val reader = new BufferedReader(new FileReader(instancesFile))
        
        val instances = if(getFileExtension(instancesFile.getName()) == "json") {
            throw new RuntimeException("Use method genClassifier(File, File, List[String] => Boolean) to define a class")
        } else if(getFileExtension(instancesFile.getName()) == "arff") {
            new Instances(reader)
        } else {
            throw new RuntimeException("Invalid file extension")
        }
        
        instances.setClassIndex(0)
        
        val classifier = classifierConfig()
        classifier.buildClassifier(instances)
        
        saveClassifier(classifier, outputFile)
    }
    
    def genClassifier(source: ArffJsonInstancesSource, targetClass: (List[String] => Boolean)): Classifier = {
        val instances = {
            val classAttribute = (inst: ArffJsonInstance) => if(targetClass(inst.mscClasses)) "yes" else "no"
            new ArffJsonInstances(source, List(Pair(new NominalArffJsonAttribute("target_class", List("no", "yes")), classAttribute))).instances
        }
        
        instances.setClassIndex(0)
        
        val classifier = classifierConfig()
        classifier.buildClassifier(instances)
        
        classifier
    }
    
    def genClassifier(source: ArffJsonInstancesSource, targetClass: (List[String] => Boolean), outputFile: File) {
        val classifier = genClassifier(source, targetClass)
        saveClassifier(classifier, outputFile)
    }
}




