package script

import common.FileManager
import common.Dispatcher
import common.Dispatcher.Quit
import parser.History
import classifier.CategoryIs
import filter.VectorFromDictFilter
import classifier.CategorizationHierarchy
import filter.TfIdfFilter
import common.TrainTuningTestSetSelection
import parser.ArffJsonInstancesSource
import common.Path
import classifier.CategoryIsMsc
import classifier.BalancedTrainSetSelection
import model.RawClassification
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import classifier.Learner
import filter.NormalizeVectorFilter
import filter.SvdLibCLsiFilter
import filter.SimpleTextToFeatureVectorFilter
import classifier.CategoryIsMscSome
import classifier.CategoryIsMscMain

object EvaluateMethodsInDifferentSettings {
    def main(args: Array[String]) {
        try {
            common.Path.rootFolder = "data_filter_irrelevant_papers"
                
            println("calculate statistics...")
            val corpusFilename = "corpus.json"
            
            val corpus = ArffJsonInstancesSource(Path.rootFolder + "/arffJson/corpus.json")
            val ((trainSet, tuningSet, testSet), c) = TrainTuningTestSetSelection.getSets(100, "prod", corpus, (0.7, 0.3, 0.0))
            
            val lsiDims = 250
            
            def jsvmLearner(stemming: Boolean, normalized: Boolean, tfidf: Boolean, lsi: Boolean): Learner = {
                val history = (stemming, normalized, tfidf, lsi) match {
                    case (_, false, false, false) => new History()
                        with AbstractTitleConcat
                        with SimpleTextToFeatureVectorFilter.Appendix {
                            override val applyStemming: Boolean = stemming
                        }
                    case (_, false, false, true) => new History()
                        with AbstractTitleConcat
                        with SimpleTextToFeatureVectorFilter.Appendix
                        with SvdLibCLsiFilter.Appendix {
                            override val applyStemming: Boolean = stemming
                            val numLsiDims = lsiDims
                        }
                    case (_, false, true, false) => new History()
                        with AbstractTitleConcat
                        with SimpleTextToFeatureVectorFilter.Appendix
                        with TfIdfFilter.Appendix {
                            override val applyStemming: Boolean = stemming
                        }
                    case (_, false, true, true) => new History()
                        with AbstractTitleConcat
                        with SimpleTextToFeatureVectorFilter.Appendix
                        with TfIdfFilter.Appendix
                        with SvdLibCLsiFilter.Appendix {
                            override val applyStemming: Boolean = stemming
                            val numLsiDims = lsiDims
                        }
                    case (_, true, false, false) => new History()
                        with AbstractTitleConcat
                        with SimpleTextToFeatureVectorFilter.Appendix
                        with NormalizeVectorFilter.Appendix {
                            override val applyStemming: Boolean = stemming
                        }
                    case (_, true, false, true) => new History()
                        with AbstractTitleConcat
                        with SimpleTextToFeatureVectorFilter.Appendix
                        with SvdLibCLsiFilter.Appendix
                        with NormalizeVectorFilter.Appendix {
                            override val applyStemming: Boolean = stemming
                            val numLsiDims = lsiDims
                        }
                    case (_, true, true, false) => new History()
                        with AbstractTitleConcat
                        with SimpleTextToFeatureVectorFilter.Appendix
                        with TfIdfFilter.Appendix
                        with NormalizeVectorFilter.Appendix {
                            override val applyStemming: Boolean = stemming
                        }
                    case (_, true, true, true) => new History()
                        with AbstractTitleConcat
                        with SimpleTextToFeatureVectorFilter.Appendix
                        with TfIdfFilter.Appendix
                        with SvdLibCLsiFilter.Appendix
                        with NormalizeVectorFilter.Appendix {
                            override val applyStemming: Boolean = stemming
                            val numLsiDims = lsiDims
                        }
                }
                
                SvmLightJniLearner(
                    history,
                    BalancedTrainSetSelection(Some(10000))
                )
            }
            
            val targetCategories = common.Common.topClasses.map(CategoryIsMscMain.top(_))
            
            for(stemming <- List(true, false); normalized <- List(true, false); tfidf <- List(true, false); lsi <- List(true, false)) {
                evaluateLearner(
                    targetCategories, 
                    (_ => jsvmLearner(stemming, normalized, tfidf, lsi)), 
                    trainSet, 
                    tuningSet, "main-" + 
                        ((if(stemming) "" else "not-") + "stemming") + "_" + 
                        ((if(normalized) "" else "not-") + "normalized") + "_" + 
                        ((if(tfidf) "" else "not-") + "tfidf") + "_" + 
                        ((if(lsi) "" else "not-") + "lsi")
                )
            }
        } catch {
            case ex: Throwable => {
                ex.printStackTrace()
            }
        } finally {
            FileManager.quit
        }
    }
    
    def evaluateEveryPermutation(learner: Learner, categories: List[CategoryIsMsc], trainSet: ArffJsonInstancesSource, testSet: ArffJsonInstancesSource, outputFilename: String) {
        val categoryCandidates = (for(i <- 1 to categories.length) yield categories.combinations(i)).flatten
        
        val fileWriter = new BufferedWriter(new FileWriter(new File("reports/" + outputFilename)))
        val buffer = new StringBuilder()
        for(categoryCandidate <- categoryCandidates) {
            val r = learner.classifications(trainSet, testSet, CategoryIs.oneOf(categoryCandidate))
            
            val precRecPoints = RawClassification.precRecGraphPoints(r)
            val bestF = ApplyFinalClassifier.bestFmeasure(precRecPoints)
            val reportStr = "{" + 
                "\"candidate\" : [" + categoryCandidate.map(_.filenameExtension).mkString(", ") + "], " + 
                "\"bestF\" : " + bestF + ", " +  
                "\"precRecGraphPoints\" : " + "[" + precRecPoints.map(p => p._2).mkString(",") + "]" +
            "}"
            
            buffer.append(reportStr + "\n")
        }
        fileWriter.write(buffer.toString)
        fileWriter.close
    }
    
    def evaluateLearner(categories: List[CategoryIsMsc], learner: (CategoryIsMsc => Learner), trainSet: ArffJsonInstancesSource, testSet: ArffJsonInstancesSource, outputFilename: String) {
        var fMeasureSum = 0.0
        val buffer = new StringBuilder()
        
        for((cat, i) <- categories.zipWithIndex) {
            ApplyFinalClassifier.printProgress(i, categories.size)
            val r = learner(cat).classifications(trainSet, testSet, cat)
            
            val (reportStr, bestF) = reportData(cat, r)
            println(reportStr)
            buffer.append(reportStr + "\n")
            fMeasureSum += bestF
        }
        
        val fileWriter = new BufferedWriter(new FileWriter(new File(common.Path.rootFolder + "/reports/" + outputFilename)))
        fileWriter.write("macro f-measure: " + (fMeasureSum / categories.size) + "\n")
        fileWriter.write(buffer.toString)
        fileWriter.close
    } 
    
    def reportData(cat: CategoryIsMsc, classifications: List[RawClassification]) = {
        val precRecPoints = RawClassification.precRecGraphPoints(classifications)
        val bestF = ApplyFinalClassifier.bestFmeasure(precRecPoints)
        val reportStr = "{" + 
            "\"topClass\" : \"" + cat.topClass.get + "\", " + 
            "\"bestF\" : " + bestF + ", " +  
            "\"precRecGraphPoints\" : " + "[" + precRecPoints.map(p => p._2).mkString(",") + "]" +
        "}"
            
        (reportStr, bestF)
    }
}