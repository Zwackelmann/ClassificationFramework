package filter

import parser.ArffJsonInstancesSource
import feature_scoreing.OddsRatio
import classifier.TargetClassDefinition
import java.io.File
import parser.History

object OddsRatioFilter2 {
    def apply(targetClassDef: TargetClassDefinition, numOrDims: Int) = new StorableFilterFactory() {
        def apply(trainBase: ArffJsonInstancesSource) = {
            new OddsRatioFilter2(trainBase, targetClassDef, numOrDims)
        }
        
        def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[OddsRatioFilter2]
        
        val historyAppendix = "or2-" + numOrDims + "-" + targetClassDef.filenameExtension
    }
    
    @serializable
    trait Appendix extends History {
        val numOrDims: Int
        abstract override def apply(targetClassDef: TargetClassDefinition) = super.apply(targetClassDef) :+ OddsRatioFilter2(targetClassDef, numOrDims)
    }
}

@serializable
class OddsRatioFilter2(trainBase: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, numOrDims: Int) extends GlobalFilter {
    val featureScoring = new OddsRatio(trainBase, targetClassDef)
    
    def applyFilter(inst: ArffJsonInstancesSource) = {
        val bestFeatures = featureScoring.rankedFeatureList
        val chosenFeatures = bestFeatures.take(numOrDims).map(_._1)
        inst.project(
            chosenFeatures
        )
    }
}