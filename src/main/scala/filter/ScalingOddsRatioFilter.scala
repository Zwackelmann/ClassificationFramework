package filter
import classifier.CategoryIs
import parser.ArffJsonInstancesSource
import java.io.File
import feature_scoreing.OddsRatio
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonHeader
import format.arff_json.SparseArffJsonInstance
import parser.History

object ScalingOddsRatioFilter {
    def apply(categoryIs: CategoryIs, orThreshold: Double, numWorst: Int, shift: Double) = new FilterFactory() with Loadable[ScalingOddsRatioFilter] {
        def apply(trainBase: ArffJsonInstancesSource) = {
            new ScalingOddsRatioFilter(trainBase, categoryIs, orThreshold, numWorst, shift)
        }
        
        val historyAppendix = "sor-" + orThreshold + "-" + numWorst + "-" + shift + "-" + categoryIs.filenameExtension
    }
    
    @serializable
    trait Appendix extends History {
        val orThreshold: Double
        val numWorst: Int
        val shift: Double
        abstract override def apply(categoryIs: CategoryIs) = super.apply(categoryIs) :+ ScalingOddsRatioFilter(categoryIs, orThreshold, numWorst, shift)
    }
    
    val maxScore = 100
    def score(oldVal: Double, score: Double, shift: Double) = {
        oldVal * math.log(
            math.min(
                maxScore,
                (if(score > 1.0) score else if(score > 0.0) 1 / score else maxScore)
            )
        )
    } + shift
}

@serializable
class ScalingOddsRatioFilter(trainBase: ArffJsonInstancesSource, categoryIs: CategoryIs, orThreshold: Double, numWorst: Int, shift: Double) extends GlobalFilter {
    import ScalingOddsRatioFilter._
    
    val featureScoring = new OddsRatio(trainBase, categoryIs)
    
    def applyFilter(inst: ArffJsonInstancesSource) = {
        val bestFeatures = featureScoring.rankedFeatureList
        val chosenFeatures = bestFeatures.takeWhile(_._2 > orThreshold) ++ bestFeatures.reverse.take(numWorst)
        
        val chosenIndexes = chosenFeatures.map(_._1)
        val featureScores = (0 until chosenFeatures.size).zip(chosenFeatures.map(_._2)).toMap
        println(chosenFeatures.size + " features selected for sor filter")
        
        inst.map((inst: ArffJsonInstance) => {
                val data = (for((key, value) <- inst.project(chosenIndexes).sparseData) yield {
                    key -> score(value, featureScores(key), shift)
                })
                new SparseArffJsonInstance(inst.id, inst.categories, data, chosenFeatures.size)
            },
            (header: ArffJsonHeader) => new ArffJsonHeader(
                header.relationName, 
                chosenIndexes.map(feature => header.attributes(feature))
            )
        )
    }
}






















