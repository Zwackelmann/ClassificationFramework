package filter
import parser.ArffJsonInstancesSource
import filter.feature_scoreing.OddsRatio
import classifier.TargetClassDefinition
import java.io.File
import classifier.TopClassIs
import format.arff_json.HistoryItem

object OddsRatioFilter {
    def apply(targetClassDef: TargetClassDefinition, numDims: Int) = new StorableFilterFactory() {
        def apply(trainBase: ArffJsonInstancesSource) = {
            new OddsRatioFilter(trainBase, numDims, this)
        }
        
        def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[OddsRatioFilter]
        
        val historyAppendix = "or-" + numDims + "-" + targetClassDef.filenameExtension
    }
}

@serializable
class OddsRatioFilter(trainBase: ArffJsonInstancesSource, numDims: Int, val historyAppendix: HistoryItem) extends Filter {
    val featureScoring = new OddsRatio(trainBase)
    
    def applyFilter(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = {
        println("use odds ratio filter on " + inst.contentDescription)
        
        val bestFeatures = featureScoring.rankedFeatureList(targetClassDef.asInstanceOf[TopClassIs].targetClass.toInt) // hacked...
        
        inst.project(
            bestFeatures.map(_._1).take(numDims),
            historyAppendix
        )
    }
}









