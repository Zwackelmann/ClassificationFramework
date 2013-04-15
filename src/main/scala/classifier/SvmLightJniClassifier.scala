package classifier

import parser.ArffJsonInstancesSource
import model.RawClassification
import format.arff_json.ArffJsonInstance
import format.arff_json.SparseArffJsonInstance
import format.arff_json.DenseArffJsonInstance
import format.arff_json.ArffJsonInstances
import format.arff_json.NominalArffJsonAttribute
import weka.classifiers.{Classifier => WekaInternalClassifier}
import java.io.File
import format.arff_json.ArffJsonHeader
import parser.ContentDescription
import parser.ContentDescribable
import jnisvmlight.LabeledFeatureVector
import jnisvmlight.SVMLightInterface
import jnisvmlight.TrainingParameters
import jnisvmlight.SVMLightModel
import common.FileManager
import FileManager.Protocol._

object SvmLightJniClassifier {
    SVMLightInterface.SORT_INPUT_VECTORS = false
    
    def load(fullFilename: String, parent: Option[Learner] = None) = {
        val savedObject = (FileManager !? ReceiveFile(fullFilename, true)) match {
            case AcceptReceiveFile(file) => {
                common.ObjectToFile.readObjectFromFile(file).asInstanceOf[Array[Any]]
            }
            case FileNotExists => throw new RuntimeException(fullFilename + " does not exist")
            case Error(msg) => throw new RuntimeException(msg)
        }
        
        val model = savedObject(0).asInstanceOf[SVMLightModel]
        val trainBaseCd = savedObject(1).asInstanceOf[Option[ContentDescription]]
        val targetClassDef = savedObject(2).asInstanceOf[CategoryIs]
        
        new SvmLightJniClassifier(model, trainBaseCd, targetClassDef, parent)
    }
    
    def apply(mappedInst: ArffJsonInstancesSource, categoryIs: CategoryIs, parent: Option[Learner]) = {
        val model = builtModel(mappedInst, categoryIs)
        new SvmLightJniClassifier(
            model,
            mappedInst match { case cd: ContentDescribable => Some(cd.contentDescription) case _ => None },
            categoryIs,
            parent
        )
    }
    
    def builtModel(mappedInst: ArffJsonInstancesSource, categoryIs: CategoryIs) = {
        val trainData = mappedInst.map(i => inst2LabeledFeatureVector(i, categoryIs)).toArray
        
        val model = {
            val tp: TrainingParameters = new TrainingParameters()
            tp.getLearningParameters().verbosity = 0
            
            val trainer = new SVMLightInterface()
            trainer.trainModel(trainData, tp)
        }
        
        model
    }
    
    def inst2LabeledFeatureVector(inst: ArffJsonInstance, categoryIs: CategoryIs) = {
        val numNonZeroFeatures = inst match {
            case sp: SparseArffJsonInstance => sp.dataMap.size
            case d: DenseArffJsonInstance => d.dataList.size
        }
        
        val (indexes, values) = inst match {
            case sp: SparseArffJsonInstance => 
                val x = (sp.sparseData.toList.sortBy(_._1)).unzip
                (x._1.map(_ + 1), x._2)
            case d: DenseArffJsonInstance => ((1 until (numNonZeroFeatures + 1)), d.data)
        }
        
        val isTarget = categoryIs(inst.categories)
        new LabeledFeatureVector((if(isTarget.isDefined && isTarget.get) 1.0 else if(isTarget.isDefined) -1.0 else 0.0), indexes.toArray, values.toArray)
    }
}

class SvmLightJniClassifier(val model: SVMLightModel, val trainBaseCD: Option[ContentDescription], categoryIs: CategoryIs, parent: Option[Learner]) extends Classifier(trainBaseCD, categoryIs, parent) {
    import SvmLightJniClassifier._
    
    def calculateClassifications(mappedInst: ArffJsonInstancesSource) = mappedInst.map(inst => { 
    	val isTarget = targetClassDef(inst.categories)
        
        new RawClassification(
    	        inst.id, 
    	        model.classify(inst2LabeledFeatureVector(inst, targetClassDef)), 
    	        (if(isTarget.isDefined && isTarget.get) 1.0 else if(isTarget.isDefined) -1.0 else 0.0)
        )}
    )
    
    def save(fullFilename: String) {
        val objToSave = Array[Any](
            model,
            trainBaseCD,
            targetClassDef
        )
        
        (FileManager !? CreateFile(fullFilename, true, (file) => {
            common.ObjectToFile.writeObjectToFile(objToSave, file)
        }))
    }
    
    override def toString = "SvmLightJniClassifier(trainBase: " + trainBaseContentDescription + ", targetClassDef: " + targetClassDef + ")"
}






























