package filter
import parser.ArffJsonInstancesSource
import filter.feature_scoreing.OddsRatio
import classifier.TargetClassDefinition
import java.io.File
import classifier.TopClassIs
import format.arff_json.HistoryItem
import parser.ArffJsonInstancesMapping

object OddsRatioFilter {
    def apply(targetClassDef: TargetClassDefinition, orThreshold: Double) = new StorableFilterFactory() {
        def apply(trainBase: ArffJsonInstancesSource) = {
            new OddsRatioFilter(trainBase, targetClassDef, orThreshold, this)
        }
        
        def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[OddsRatioFilter]
        
        val historyAppendix = "or-" + orThreshold + "-" + targetClassDef.filenameExtension
    }
}

@serializable
class OddsRatioFilter(trainBase: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, orThreshold: Double, val historyAppendix: HistoryItem) extends GlobalFilter {
    val featureScoring = new OddsRatio(trainBase, targetClassDef)
    
    def applyFilter(inst: ArffJsonInstancesSource) = {
        println("use odds ratio filter on " + inst.contentDescription)
        
        val bestFeatures = featureScoring.rankedFeatureList
        
        inst.project(
            bestFeatures.map(_._1).takeWhile(_ > orThreshold),
            historyAppendix
        )
    }
}









