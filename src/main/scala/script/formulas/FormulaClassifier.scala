package script.formulas

import parser.ArffJsonInstancesSource
import common.TrainTuningTestSetSelection
import common.Path
import classifier.CategoryIs
import script.SvmLightJniLearner
import parser.History
import classifier.BalancedTrainSetSelection
import script.ApplyFinalClassifier
import common.FileManager
import model.RawClassification
import filter.TfIdfFilter
import filter.OddsRatioFilter
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File
import classifier.Classifier
import scala.collection.mutable
import filter.NormalizeVectorFilter
import filter.OddsRatioFilter2
import classifier.CategoryIsMSC

object FormulaClassifier {
    def main(args: Array[String]) {
        try {
            common.Path.rootFolder = "cikm"
            println("calculate statistics...")
            
            val corpus = ArffJsonInstancesSource(Path.rootFolder + "/arffJson/corpus.json")
            val ((trainSet, tuningSet, testSet), _) = TrainTuningTestSetSelection.getSets(100, "cikm", corpus, (0.7, 0.3, 0.0))
            
            val histogram = new mutable.HashMap[String, Int]() {
                override def default(key: String) = 0
            }
            
            for(doc <- trainSet; cat <- doc.categories.map(c => c.substring(0, 2)).distinct) {
                histogram(cat) = histogram(cat) + 1
            }
            
            val layer = 1
            val minOccurences = 100
            
            val learner = List((SvmLightJniLearner(
                new History(),
                BalancedTrainSetSelection(Some(10000))
            ), "flat"), (SvmLightJniLearner(
                new History() with TfIdfFilter.Appendix,
                BalancedTrainSetSelection(Some(10000))
            ), "tfidf"), (SvmLightJniLearner(
                new History() with TfIdfFilter.Appendix with NormalizeVectorFilter.Appendix,
                BalancedTrainSetSelection(Some(10000))
            ), "tfidf-norm"), (SvmLightJniLearner(
                new History() with OddsRatioFilter.Appendix {
                    val numWorst = 0
                    val orThreshold = 1.0
                },
                BalancedTrainSetSelection(Some(10000))
            ), "or-1.0"), (SvmLightJniLearner(
                new History() with NormalizeVectorFilter.Appendix with OddsRatioFilter.Appendix {
                    val numWorst = 0
                    val orThreshold = 1.0
                },
                BalancedTrainSetSelection(Some(10000))
            ), "norm-or-1.0"))
            
            val firstLevelClasses = 
                // List("05", "11", "14", "16", "20", "30", "32", "34", "35", "45", "53", "60", "68")
                common.Common.topClasses
                .map(CategoryIsMSC.top(_))
            
            for((l, name) <- learner) {
                println("start " + name)
                val b = new StringBuffer()
                var count = 0
                var macroFMeasureAccu = 0.0
                
                for((cat, i) <- firstLevelClasses.zipWithIndex) {
                    ApplyFinalClassifier.printProgress(i, firstLevelClasses.size)
                    
                    val r = l.classifications(trainSet, tuningSet, cat)
                    
                    val precRecPoints = RawClassification.precRecGraphPoints(r)
                    val bestF = bestFmeasure(precRecPoints)
                    
                    val reportStr = "{" + 
                        "\"topClass\" : \"" + cat.topClass.get + "\", " + 
                        "\"numTrainInstances\" : " + histogram(cat.filenameExtension.substring(0, 2)) + ", " + 
                        "\"bestF\" : " + bestF + ", " +  
                        "\"precRecGraphPoints\" : " + "[" + precRecPoints.map(p => p._2).mkString(",") + "]" +
                    "}\n"
                    
                    println(reportStr)
                    b.append(reportStr)
                    count += 1
                    macroFMeasureAccu += bestF
                }
            
                val writer = new BufferedWriter(new FileWriter(new File("all-features-flat")))
                writer.write("[\"makro-f\" : " + (macroFMeasureAccu / count) + "]\n")
                writer.write(b.toString())
                
                writer.close
            }
            
            println("all finished")
            
        } catch {
            case ex: Throwable => {
                ex.printStackTrace()
            }
        } finally {
            FileManager.quit
        }
    }
    
    def fMeasure(prec: Double, rec: Double) = (2 * prec * rec) / (prec + rec)
    
    def bestFmeasure(seq: Seq[Pair[Double, Double]]) = {
        val seq2 = seq.filter(x => !fMeasure(x._2, x._1).isNaN())
        
        if(seq2.isEmpty) Double.NaN
        else {
            val p = seq2.maxBy(x => fMeasure(x._2, x._1))
            (2 * p._1 * p._2) / (p._1 + p._2)
        }
    }
}























