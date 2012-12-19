package filter
import parser.ArffJsonInstancesSource
import filter.feature_scoreing.OddsRatio
import classifier.TargetClassDefinition
import java.io.File
import classifier.TopClassIs
import parser.History

object OddsRatioFilter {
    def apply(targetClassDef: TargetClassDefinition, orThreshold: Double, numWorst: Int) = new StorableFilterFactory() {
        def apply(trainBase: ArffJsonInstancesSource) = {
            new OddsRatioFilter(trainBase, targetClassDef, orThreshold, numWorst)
        }
        
        def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[OddsRatioFilter]
        
        val historyAppendix = "or-" + orThreshold + "-" + numWorst + "-" + targetClassDef.filenameExtension
    }
    
    @serializable
    trait Appendix extends History {
        val orThreshold: Double
        val numWorst: Int
        abstract override def apply(targetClassDef: TargetClassDefinition) = super.apply(targetClassDef) :+ OddsRatioFilter(targetClassDef, orThreshold, numWorst)
    }
}

@serializable
class OddsRatioFilter(trainBase: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, orThreshold: Double, numWorst: Int) extends GlobalFilter {
    val featureScoring = new OddsRatio(trainBase, targetClassDef)
    
    def applyFilter(inst: ArffJsonInstancesSource) = {
        val bestFeatures = featureScoring.rankedFeatureList
        val chosenFeatures = bestFeatures.takeWhile(_._2 > orThreshold).map(_._1) ++ bestFeatures.reverse.take(numWorst).map(_._1)
        println(chosenFeatures.size + " features selected for or filter")
        val returnInst = inst.project(
            chosenFeatures
        )
        returnInst
    }
}









