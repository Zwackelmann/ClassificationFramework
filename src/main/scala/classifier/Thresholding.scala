package classifier
import parser.ArffJsonInstancesSource
import parser.ContentDescribable
import parser.ContentDescription
import model.RawClassification
import common.Path

object Thresholding {
    case class Threshold(val t: Double)
}

trait Thresholding extends Learner {
    import Thresholding._
    
    abstract override def classifications(inst: ArffJsonInstancesSource with ContentDescribable, targetClassDef: TargetClassDefinition, varifyClassifications: Boolean = false) = {
        val c = super.classifications(inst, targetClassDef, varifyClassifications)
        val thresholdPath = {
            val trainBaseCd = ContentDescription(inst.contentDescription.base, ContentDescription.TrainSet, targetHistory(targetClassDef))
            val targetCd = ContentDescription(inst.contentDescription.base, inst.contentDescription.set, targetHistory(targetClassDef))
            
            val resultsFilename = Learner.resultsFilename(trainBaseCd, targetClassDef, Some(this), targetCd)
            Path.thresholdsPath / resultsFilename
        }
        
        val threshold = if(!thresholdPath.exists()) {
            val classifications = {
            	val c = super.classificationsIfCached(inst.contentDescription.toTuning, targetClassDef)
            	c match {
            	    case Some(c) => c
            	    case None => super.classifications(mapInstances(inst, targetClassDef, Some(ContentDescription.TuningSet)), targetClassDef)
            	}
            }
            
            // TODO find out why this crashes in 1 of 10000 cases.... 
            val bestThreshold = try {
                RawClassification.findBestThreshold(RawClassification.normalize(classifications))
            } catch {
                case _ => 0.0
            }
            common.ObjectToFile.writeObjectToFile(bestThreshold, thresholdPath)
            bestThreshold
        } else {
            common.ObjectToFile.readObjectFromFile(thresholdPath).asInstanceOf[Double]
        }
        
        RawClassification.withThreshold(c, threshold)
    }
}






