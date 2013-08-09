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
import jnisvmlight.LabeledFeatureVector
import jnisvmlight.SVMLightInterface
import jnisvmlight.TrainingParameters
import jnisvmlight.SVMLightModel
import common.FileManager
import FileManager.Protocol._
import format.arff_json.SparseData
import format.arff_json.DenseData
import format.arff_json.SparseData
import format.arff_json.DenseData

object SvmLightJniClassifier {
    SVMLightInterface.SORT_INPUT_VECTORS = false
    
    def load(fullFilename: String, parent: Option[Learner] = None) = {
        val savedObject = (FileManager !? ReceiveFile(fullFilename)) match {
            case AcceptReceiveFile(file) => {
                common.ObjectToFile.readObjectFromFile(file).asInstanceOf[Array[Any]]
            }
            case FileNotExists => throw new RuntimeException(fullFilename + " does not exist")
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
        val numAttributes = mappedInst.header.numAttributes
        val trainData: Array[LabeledFeatureVector] = (
            mappedInst.map(i => inst2LabeledFeatureVector(i, categoryIs)).toList :+
            dummyFeatureVector(numAttributes) // checked if this changes anything in the result.
            // I tested the impact on a quite small amount of documents, because if there is an effect then
            // it should be biggest when a small amount of training documents are involved.
            // In the tests, no difference in any classifier could be registered with an accuracy of ten digits 
            // after the decimal point...
        ).toArray
        
        val model = {
            val tp: TrainingParameters = new TrainingParameters()
            tp.getLearningParameters().verbosity = 0
            
            val trainer = new SVMLightInterface()
            trainer.trainModel(trainData, tp)
        }
        
        model
    }
    
    def dummyFeatureVector(size: Int) = {
        val indexes = (1 to size).toArray
        val values = (1 to size).map(c => 0.0).toArray
        new LabeledFeatureVector(-1.0, indexes, values)
    }
    
    def inst2LabeledFeatureVector(inst: ArffJsonInstance, realValue: Double): LabeledFeatureVector = {
        val numNonZeroFeatures = inst match {
            case sp: SparseData => sp.dataMap.size
            case d: DenseData => d.dataList.size
        }
        
        val (indexes: List[Int], values: List[Double]) =  try {
            inst match {
                case sp: SparseData => 
                    val x: Pair[List[Int], List[Double]] = (sp.sparseData.toList.sortBy(_._1)).unzip
                    (x._1.map(_ + 1), x._2)
                case d: DenseData => ((1 until (numNonZeroFeatures + 1)).toList, d.data)
                case x => x.getClass
            }
        } catch {
            case x: Throwable => println(inst); throw x;
        }
        
        new LabeledFeatureVector(realValue, indexes.toArray, values.toArray)
    }
    
    def inst2LabeledFeatureVector(inst: ArffJsonInstance, categoryIs: CategoryIs): LabeledFeatureVector = {
        val isTarget = categoryIs.matchesForTraining(inst.categories)
        inst2LabeledFeatureVector(inst, (if(isTarget.isDefined && isTarget.get) 1.0 else if(isTarget.isDefined) -1.0 else 0.0))
    }
}

class SvmLightJniClassifier(val model: SVMLightModel, val trainBaseCD: Option[ContentDescription], categoryIs: CategoryIs, parent: Option[Learner]) extends Classifier(trainBaseCD, categoryIs, parent) {
    import SvmLightJniClassifier._
    
    def calculateClassifications(mappedInst: ArffJsonInstancesSource) = {
        mappedInst.map(inst => { 
            val isTarget = targetClassDef.matchesForTesting(inst.categories)
            
            new RawClassification(
        	        inst.id, 
        	        model.classify(inst2LabeledFeatureVector(inst, targetClassDef)), 
        	        (if(isTarget.isDefined && isTarget.get) 1.0 else if(isTarget.isDefined) -1.0 else 0.0)
            )}
        )
    }
    
    def save(fullFilename: String) {
        val objToSave = Array[Any](
            model,
            trainBaseCD,
            targetClassDef
        )
        
        (FileManager !? CreateFile(fullFilename)) match {
            case AcceptCreateFile(fileHandle) => {
                common.ObjectToFile.writeObjectToFile(objToSave, fileHandle.file)
                fileHandle.close
            }
            case RejectCreateFile => throw new RuntimeException("could not save classifier")
        }
            
    }
    
    override def toString = "SvmLightJniClassifier(trainBase: " + trainBaseContentDescription + ", targetClassDef: " + targetClassDef + ")"
}






























