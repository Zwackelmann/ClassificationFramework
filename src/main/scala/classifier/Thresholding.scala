package classifier
import parser.ArffJsonInstancesSource
import parser.ContentDescribable
import parser.ContentDescription
import model.RawClassification
import common.Path
import common.FileManager
import FileManager.Protocol._

object Thresholding {
    case class Threshold(val t: Double)
}

trait Thresholding extends Learner {
    import Thresholding._
    
    abstract override def classifications(trainSet: ArffJsonInstancesSource, tuningSet: ArffJsonInstancesSource, cat: CategoryIs) = {
        val c = super.classifications(trainSet, tuningSet, cat)
        
        val (contentDescribableTrainSet, contentDescribableTuningSet) = (trainSet, tuningSet) match {
            case (contentDescribableTrainSet: ContentDescribable, contentDescribableTuningSet: ContentDescribable) => (contentDescribableTrainSet, contentDescribableTuningSet)
            case _ => throw new RuntimeException("thresholding with trainSet or tuningSet that are not ContentDescribable is not supported yet")
        }
        
        val thresholdPath = {
            val resultsFilename = Learner.classificationsFilename(contentDescribableTrainSet.contentDescription, cat, Some(this), contentDescribableTuningSet.contentDescription)
            Path.thresholdsPath / resultsFilename
        }
        
        val threshold = (FileManager !? CreateOrReceiveFile(thresholdPath)) match {
            case AcceptReceiveFile(file) => {
                common.ObjectToFile.readObjectFromFile(file).asInstanceOf[Double]
            }
            case AcceptCreateFile(fileHandle) => { 
                val classifications = super.classifications(trainSet, tuningSet, cat)
                
                // TODO find out why this crashes in 1 of 10000 cases.... 
                val bestThreshold = try {
                    RawClassification.findBestThreshold(RawClassification.normalize(classifications))
                } catch {
                    case _: Throwable => 0.0
                }
                common.ObjectToFile.writeObjectToFile(bestThreshold, fileHandle.file)
                fileHandle.close
                bestThreshold
            }
        }
        
        RawClassification.withThreshold(c, threshold)
    }
}





















