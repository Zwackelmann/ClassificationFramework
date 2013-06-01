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
import classifier.CategoryIsMSC
import classifier.BalancedTrainSetSelection
import model.RawClassification
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import classifier.Learner
import filter.NormalizeVectorFilter
import external.GensimLsiFilter

object ApplyFinalClassifier2 {
    def main(args: Array[String]) {
        try {
            common.Path.rootFolder = "data_evaluate_if_choosing_only_the_main_class_is_better"
            println("calculate statistics...")
            val corpusFilename = "corpus.json"
            
            val corpus = ArffJsonInstancesSource(Path.rootFolder + "/arffJson/" + corpusFilename)
            
            val ((trainSet, tuningSet, testSet), c) = TrainTuningTestSetSelection.getSets(100, "prod", corpus, (0.7, 0.3, 0.0))
            
            val consideredCategories = c map (c => CategoryIsMSC(c))
            val layer = 1
            val minOccurences = 100
            
            def jsvmLearner(stemming: Boolean, normalized: Boolean, tfidf: Boolean, lsi: Boolean): Learner = {
                val history = (stemming, normalized, tfidf, lsi) match {
                    case (_, false, false, false) => new History()
                        with AbstractTitleConcat
                        with VectorFromDictFilter.Appendix {
                            val confName = (if(stemming) "conf11" else "conf10")
                            override val minOcc = 3
                        }
                    case (_, false, false, true) => new History()
                        with AbstractTitleConcat
                        with VectorFromDictFilter.Appendix
                        with GensimLsiFilter.Appendix {
                            val confName = (if(stemming) "conf11" else "conf10")
                            override val minOcc = 3
                            val numLsiDims = 1000
                        }
                    case (_, false, true, false) => new History()
                        with AbstractTitleConcat
                        with VectorFromDictFilter.Appendix
                        with TfIdfFilter.Appendix {
                            val confName = (if(stemming) "conf11" else "conf10")
                            override val minOcc = 3
                        }
                    case (_, false, true, true) => new History()
                        with AbstractTitleConcat
                        with VectorFromDictFilter.Appendix
                        with TfIdfFilter.Appendix
                        with GensimLsiFilter.Appendix {
                            val confName = (if(stemming) "conf11" else "conf10")
                            override val minOcc = 3
                            val numLsiDims = 1000
                        }
                    case (_, true, false, false) => new History()
                        with AbstractTitleConcat
                        with VectorFromDictFilter.Appendix
                        with NormalizeVectorFilter.Appendix {
                            val confName = (if(stemming) "conf11" else "conf10")
                            override val minOcc = 3
                        }
                    case (_, true, false, true) => new History()
                        with AbstractTitleConcat
                        with VectorFromDictFilter.Appendix
                        with GensimLsiFilter.Appendix
                        with NormalizeVectorFilter.Appendix {
                            val confName = (if(stemming) "conf11" else "conf10")
                            override val minOcc = 3
                            val numLsiDims = 1000
                        }
                    case (_, true, true, false) => new History()
                        with AbstractTitleConcat
                        with VectorFromDictFilter.Appendix
                        with TfIdfFilter.Appendix
                        with NormalizeVectorFilter.Appendix {
                            val confName = (if(stemming) "conf11" else "conf10")
                            override val minOcc = 3
                        }
                    case (_, true, true, true) => new History()
                        with AbstractTitleConcat
                        with VectorFromDictFilter.Appendix
                        with TfIdfFilter.Appendix
                        with GensimLsiFilter.Appendix
                        with NormalizeVectorFilter.Appendix {
                            val confName = (if(stemming) "conf11" else "conf10")
                            override val minOcc = 3
                            val numLsiDims = 1000
                        }
                }
                
                SvmLightJniLearner(
                    history,
                    BalancedTrainSetSelection(Some(10000))
                )
            }
            
            val thirdLevelClasses = consideredCategories
            val secondLevelClasses = thirdLevelClasses.map(_.parent)
            val firstLevelClasses = secondLevelClasses.map(_.parent)
            
            val targetCategories = (layer match {
                case 1 => firstLevelClasses
                case 2 => secondLevelClasses
                case 3 => thirdLevelClasses
            }).toList.sortBy(_.filenameExtension)
            
            for(stemming <- List(true, false); normalized <- List(true, false); tfidf <- List(true, false); lsi <- List(true, false)) {
                evaluateLearner(
                    targetCategories, 
                    (_ => jsvmLearner(stemming, normalized, tfidf, lsi)), 
                    trainSet, 
                    tuningSet, 
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
    
    def evaluateEveryPermutation(learner: Learner, categories: List[CategoryIsMSC], trainSet: ArffJsonInstancesSource, testSet: ArffJsonInstancesSource, outputFilename: String) {
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
    
    def evaluateLearner(categories: List[CategoryIsMSC], learner: (CategoryIsMSC => Learner), trainSet: ArffJsonInstancesSource, testSet: ArffJsonInstancesSource, outputFilename: String) {
        var fMeasureSum = 0.0
        val buffer = new StringBuilder()
        
        for((cat, i) <- categories.zipWithIndex) {
            ApplyFinalClassifier.printProgress(i, categories.size)
            val r = learner(cat).classifications(trainSet, testSet, cat)
            
            val (reportStr, bestF) = reportData(cat, r)
            buffer.append(reportStr + "\n")
            fMeasureSum += bestF
        }
        
        val fileWriter = new BufferedWriter(new FileWriter(new File("reports/" + outputFilename)))
        fileWriter.write("macro f-measure: " + (fMeasureSum / categories.size) + "\n")
        fileWriter.write(buffer.toString)
        fileWriter.close
    } 
    
    def reportData(cat: CategoryIsMSC, classifications: List[RawClassification]) = {
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