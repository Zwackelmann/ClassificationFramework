package filter
import parser.ArffJsonInstancesSource
import filter.feature_scoreing.OddsRatio
import classifier.CategoryIs
import java.io.File
import parser.History

object OddsRatioFilter {
    def apply(categoryIs: CategoryIs, orThreshold: Double, numWorst: Int) = new FilterFactory() with Loadable[OddsRatioFilter] {
        def apply(trainBase: ArffJsonInstancesSource) = {
            new OddsRatioFilter(trainBase, categoryIs, orThreshold, numWorst)
        }
        
        val historyAppendix = "or-" + orThreshold + "-" + numWorst + "-" + categoryIs.filenameExtension
    }
    
    @serializable
    trait Appendix extends History {
        val orThreshold: Double
        val numWorst: Int
        abstract override def apply(categoryIs: CategoryIs) = super.apply(categoryIs) :+ OddsRatioFilter(categoryIs, orThreshold, numWorst)
    }
}

@serializable
class OddsRatioFilter(trainBase: ArffJsonInstancesSource, categoryIs: CategoryIs, orThreshold: Double, numWorst: Int) extends GlobalFilter {
    val featureScoring = new OddsRatio(trainBase, categoryIs)
    
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









