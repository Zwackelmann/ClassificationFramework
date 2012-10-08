package filter
import classifier.TargetClassDefinition
import parser.ArffJsonInstancesSource
import format.arff_json.HistoryItem
import java.io.File
import feature_scoreing.OddsRatio
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonHeader
import format.arff_json.SparseArffJsonInstance

object ScalingOddsRatioFilter {
    
    def apply(targetClassDef: TargetClassDefinition, orThreshold: Double, numWorst: Int, shift: Double) = new StorableFilterFactory() {
        def apply(trainBase: ArffJsonInstancesSource) = {
            new ScalingOddsRatioFilter(trainBase, targetClassDef, orThreshold, numWorst, shift, this)
        }
        
        def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[ScalingOddsRatioFilter]
        
        val historyAppendix = "sor-" + orThreshold + "-" + numWorst + "-" + shift + "-" + targetClassDef.filenameExtension
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
class ScalingOddsRatioFilter(trainBase: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, orThreshold: Double, numWorst: Int, shift: Double, val historyAppendix: HistoryItem) extends GlobalFilter {
    import ScalingOddsRatioFilter._
    
    val featureScoring = new OddsRatio(trainBase, targetClassDef)
    
    def applyFilter(inst: ArffJsonInstancesSource) = {
        println("use scaling odds ratio filter on " + inst.contentDescription)
        
        val bestFeatures = featureScoring.rankedFeatureList
        val chosenFeatures = bestFeatures.takeWhile(_._2 > orThreshold) ++ bestFeatures.reverse.take(numWorst)
        
        val chosenIndexes = chosenFeatures.map(_._1)
        val featureScores = (0 until chosenFeatures.size).zip(chosenFeatures.map(_._2)).toMap
        println(chosenFeatures.size + " features selected for sor filter")
        
        inst.map(
            (it: Iterator[ArffJsonInstance]) => it.map(inst => {
                val data = (for((key, value) <- inst.project(chosenIndexes).sparseData) yield {
                    key -> score(value, featureScores(key), shift)
                })
                new SparseArffJsonInstance(inst.id, inst.mscClasses, data, chosenFeatures.size)
            }),
            (header: ArffJsonHeader) => new ArffJsonHeader(
                header.relationName, 
                chosenIndexes.map(feature => header.attributes(feature)),
                header.metaAttributes
            ),
            historyAppendix
        )
    }
}






















