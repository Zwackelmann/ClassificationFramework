package filter

import parser.ArffJsonInstancesSource
import feature_scoreing.OddsRatio
import classifier.CategoryIs
import java.io.File
import parser.History

object OddsRatioFilter2 {
    def apply(categoryIs: CategoryIs, numOrDims: Int) = new FilterFactory() with Loadable[OddsRatioFilter2] {
        def apply(trainBase: ArffJsonInstancesSource) = {
            new OddsRatioFilter2(trainBase, categoryIs, numOrDims) {
                override val trainingParams = Filter.trainingParams(historyAppendix, trainBase)
            }
        }
        
        val historyAppendix = "or2-" + numOrDims + "-" + categoryIs.filenameExtension
    }
    
    trait Appendix extends History with Serializable {
        val numOrDims: Int
        abstract override def apply(categoryIs: CategoryIs) = super.apply(categoryIs) :+ OddsRatioFilter2(categoryIs, numOrDims)
    }
}

abstract class OddsRatioFilter2(trainBase: ArffJsonInstancesSource, categoryIs: CategoryIs, numOrDims: Int) extends GlobalFilter with Serializable {
    val featureScoring = new OddsRatio(trainBase, categoryIs)
    
    def applyFilter(inst: ArffJsonInstancesSource) = {
        val bestFeatures = featureScoring.rankedFeatureList
        val chosenFeatures = bestFeatures.take(numOrDims).map(_._1).toSet
        inst.project(
            chosenFeatures
        )
    }
}