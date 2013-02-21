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

object SvmLightJniClassifier {
    SVMLightInterface.SORT_INPUT_VECTORS = false
    
    def load(file: File, parent: Option[Learner] = None) = {
        val savedObject = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[Array[Any]]
        val model = savedObject(0).asInstanceOf[SVMLightModel]
        val trainBaseCd = savedObject(1).asInstanceOf[Option[ContentDescription]]
        val targetClassDef = savedObject(2).asInstanceOf[TargetClassDefinition]
        
        new SvmLightJniClassifier(model, trainBaseCd, targetClassDef, parent)
    }
    
    def apply(mappedInst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, parent: Option[Learner]) = {
        val model = builtModel(mappedInst, targetClassDef)
        new SvmLightJniClassifier(
            model,
            mappedInst match { case cd: ContentDescribable => Some(cd.contentDescription) case _ => None },
            targetClassDef,
            parent
        )
    }
    
    def builtModel(mappedInst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = {
        val trainData = mappedInst.map(i => inst2LabeledFeatureVector(i, targetClassDef)).toArray
        
        val model = {
            val tp: TrainingParameters = new TrainingParameters()
            tp.getLearningParameters().verbosity = 1
            
            val trainer = new SVMLightInterface()
            trainer.trainModel(trainData, tp)
        }
        
        model
    }
    
    def inst2LabeledFeatureVector(inst: ArffJsonInstance, targetClassDef: TargetClassDefinition) = {
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
        
        new LabeledFeatureVector((if(targetClassDef(inst.categories)) 1.0 else -1.0), indexes.toArray, values.toArray)
    }
}

class SvmLightJniClassifier(val model: SVMLightModel, val trainBaseCD: Option[ContentDescription], targetClassDef: TargetClassDefinition, parent: Option[Learner]) extends Classifier(trainBaseCD, targetClassDef, parent) {
    import SvmLightJniClassifier._
    
    def calculateClassifications(mappedInst: ArffJsonInstancesSource) = mappedInst.map(inst => 
    	new RawClassification(
    	        inst.id, 
    	        model.classify(inst2LabeledFeatureVector(inst, targetClassDef)), 
    	        (if(targetClassDef(inst.categories)) 1.0 else -1.0)
    	)
    )
    
    def save(outFile: File) {
        val objToSave = Array[Any](
            model,
            trainBaseCD,
            targetClassDef
        )
        
        common.ObjectToFile.writeObjectToFile(objToSave, outFile)
    }
    
    override def toString = "SvmLightJniClassifier(trainBase: " + trainBaseContentDescription + ", targetClassDef: " + targetClassDef + ")"
}






























