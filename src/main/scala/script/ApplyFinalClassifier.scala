package script
import classifier.Learner
import parser.ArffJsonInstancesSource
import common.Path.classifierPath
import classifier.Classifier
import external.JoachimsSVMClassifier
import classifier.WekaClassifier
import model.RawClassification
import weka.classifiers.bayes.NaiveBayes
import weka.classifiers.trees.J48
import filter.ProjectionFilter
import filter.VectorFromDictFilter
import java.io.File
import filter.TfIdfFilter
import filter.NormalizeVectorFilter
import external.GensimLsiFilter
import filter.OddsRatioFilter
import filter.OddsRatioFilter2
import format.arff_json.InstancesMappings
import parser.ContentDescription
import classifier.TrainSetSelection
import weka.classifiers.meta.AdaBoostM1
import format.arff_json.ArffJsonInstance
import filter.VectorFromNGramTreeFilter
import common.Common.FileConversion._
import format.arff_json.ArffJsonHeader
import format.arff_json.SparseArffJsonInstance
import format.arff_json.DenseArffJsonInstance
import filter.ScalingOddsRatioFilter
import filter.MostFrequentTermsFilter
import filter.FilterFactory
import parser.History
import classifier.TrainSetSelectionDefinition
import classifier.TrainSetSelectionDefinition
import classifier.BalancedTrainSetSelection
import classifier.NoTrainSetSelection
import parser.ContentDescribable
import classifier.FixedTrainSetSelection
import classifier.CategoryIs
import filter.CategorySelectionFilter
import classifier.BalancedTrainSetSelection
import common.TrainTuningTestSetSelection
import classifier.Thresholding
import java.io.BufferedWriter
import java.io.FileWriter
import scala.collection.mutable
import classifier.MaxForEachSet
import classifier.PrioritizeMainClass
import weka.core.neighboursearch.LinearNNSearch
import weka.attributeSelection.ConsistencySubsetEval
import javax.swing.filechooser.FileNameExtensionFilter
import filter.ConcatFilter
import classifier.SvmLightJniClassifier
import common.Dispatcher
import common.Dispatcher.{Task, Quit}
import common.FileManager
import common.Path

object ApplyFinalClassifier {
    def main(args: Array[String]) {
        val arguments = {
            // args.toList
            List("corpus.jar", "min100", "1")
        }
        if(arguments.size != 3) {
            println("Erstes Argument: Corpus dateiname data/arffJson\nZweites Argument name des Korpus\nDrittes Argument layer für die Klassifizierung")
            exit(1)
        } 
        try {
            Path.rootFolder = "data_stemmed_termes_fuer_tbdb"
            
            println("calculate statistics...")
            val corpusFilename = "corpus.json" // args(0)
            
            val corpus = ArffJsonInstancesSource(Path.rootFolder + "/arffJson/" + corpusFilename)
            
            val ((trainSet, tuningSet, testSet), c) = TrainTuningTestSetSelection.getSets(100, arguments(1), corpus, (0.7, 0.3, 0.0))
            // val ((trainSet, tuningSet, testSet), c) = TrainTuningTestSetSelection.getSets(100, arguments(1), corpus, (0.6, 0.2, 0.2))
            
            val consideredCategories = c map (c => CategoryIs(c))
            
            val layer = arguments(2).toInt
            val minOccurences = 100
            
            val startTime = System.currentTimeMillis()
            val evaluationDataAccumulator = new EvaluationDataAccumulator()
            
            def jsvmUniLearner(cat: CategoryIs, layer: Int) = SvmLightJniLearner(
                new History() 
                        with AbstractTitleConcat
                        with CategorySelectionFilter.Appendix
                        with VectorFromDictFilter.Appendix
                        with TfIdfFilter.Appendix
                        with NormalizeVectorFilter.Appendix { 
                    
                    val selection = cat.parent
                    val confName = "conf9"
                    override val minOcc = layer match {
                        case 1 => 3
                        case 2 | 3 => 1
                        case _ => throw new RuntimeException("layer must be between 1 and 3")
                    }
                }
            )
            
            val dispatcher = new Dispatcher(1)
            dispatcher.start()
            
            val thirdLevelClasses = consideredCategories
            val secondLevelClasses = thirdLevelClasses.map(_.parent)
            val firstLevelClasses = secondLevelClasses.map(_.parent)
            
            val targetCategories = (layer match {
                case 1 => firstLevelClasses
                case 2 => secondLevelClasses
                case 3 => thirdLevelClasses
            }).toList.sortBy(_.filenameExtension)
            
            val evaluator = new EvaluationDataAccumulator
            
            for((cat, i) <- targetCategories.zipWithIndex) {
                println("start cat " + cat.filenameExtension + "...")
                printProgress(i, targetCategories.size)
                
                val learner = jsvmUniLearner(cat, layer)
                val r = learner.classifications(trainSet, tuningSet, cat)
                evaluator("svmUni", r)
                // dispatcher !? Task(() => {
                    // jsvmTitleLearner.classifications(trainSet, tibTestSet, cat)
                // })
            }
            dispatcher !? Quit
            println("all finished")
            
            val writer = new BufferedWriter(new FileWriter(new File("prec-rec-graphs-stemmed-tersm")))
            evaluator.precRecGraphs(None, writer)
            evaluator.precRecGraphs(Some(10), writer)
            writer.close
            /*launchAsynchronous(categoryGroups) { cat => {
                val minWordCount = if(layer == 1) 3 else 1
                val orTh = if(layer == 1) 5.0 else 2.0
                val maxTrainSetSize = 10000
                val numAdaBoostIterations = 20
                
                /*val c45AbstractLearner = BoostedC45Learner(
                    new History() 
                            with AbstractProjection
                            with CategorySelectionFilter.Appendix
                            with VectorFromDictFilter.Appendix
                            with NormalizeVectorFilter.Appendix
                            with OddsRatioFilter.Appendix { 
                        
                        val selection = cat.parent
                        val confName = "conf9"
                        override val minOcc = minWordCount
                        val orThreshold = orTh
                        val numWorst = 0
                    },
                    PrioritizeMainClass(maxTrainSetSize),
                    numAdaBoostIterations
                )
                
                val c45TitleLearner = BoostedC45Learner(
                    new History() 
                            with TitleProjection
                            with CategorySelectionFilter.Appendix
                            with VectorFromDictFilter.Appendix
                            with NormalizeVectorFilter.Appendix
                            with OddsRatioFilter.Appendix { 
                        
                        val selection = cat.parent
                        val confName = "conf9"
                        override val minOcc = minWordCount
                        val orThreshold = orTh
                        val numWorst = 0
                    },
                    PrioritizeMainClass(maxTrainSetSize),
                    numAdaBoostIterations
                )
                
                val svmAbstractLearner = SvmLearner(
                    new History() 
                            with AbstractProjection
                            with CategorySelectionFilter.Appendix
                            with VectorFromDictFilter.Appendix
                            with TfIdfFilter.Appendix
                            with NormalizeVectorFilter.Appendix { 
                        
                        val selection = cat.parent
                        val confName = "conf9"
                        override val minOcc = minWordCount
                    },
                    NoTrainSetSelection
                )
                
                val svmTitleLearner = SvmLearner(
                    new History() 
                            with TitleProjection
                            with CategorySelectionFilter.Appendix
                            with VectorFromDictFilter.Appendix
                            with TfIdfFilter.Appendix
                            with NormalizeVectorFilter.Appendix { 
                        
                        val selection = cat.parent
                        val confName = "conf9"
                        override val minOcc = minWordCount
                    },
                    NoTrainSetSelection
                )*/
                
                val c45AbstractLearner = BoostedC45LearnerNT(
                    new History() 
                            with AbstractProjection
                            with CategorySelectionFilter.Appendix
                            with VectorFromDictFilter.Appendix
                            with NormalizeVectorFilter.Appendix
                            with OddsRatioFilter.Appendix { 
                        
                        val selection = cat.parent
                        val confName = "conf9"
                        override val minOcc = minWordCount
                        val orThreshold = orTh
                        val numWorst = 0
                    },
                    PrioritizeMainClass(maxTrainSetSize),
                    numAdaBoostIterations
                )
                
                val c45TitleLearner = BoostedC45LearnerNT(
                    new History() 
                            with TitleProjection
                            with CategorySelectionFilter.Appendix
                            with VectorFromDictFilter.Appendix
                            with NormalizeVectorFilter.Appendix
                            with OddsRatioFilter.Appendix { 
                        
                        val selection = cat.parent
                        val confName = "conf9"
                        override val minOcc = minWordCount
                        val orThreshold = orTh
                        val numWorst = 0
                    },
                    PrioritizeMainClass(maxTrainSetSize),
                    numAdaBoostIterations
                )
                
                val svmAbstractLearner = SvmLearnerNT(
                    new History() 
                            with AbstractProjection
                            with CategorySelectionFilter.Appendix
                            with VectorFromDictFilter.Appendix
                            with TfIdfFilter.Appendix
                            with NormalizeVectorFilter.Appendix { 
                        
                        val selection = cat.parent
                        val confName = "conf9"
                        override val minOcc = minWordCount
                    },
                    NoTrainSetSelection
                )
                
                val svmTitleLearner = SvmLearnerNT(
                    new History() 
                            with TitleProjection
                            with CategorySelectionFilter.Appendix
                            with VectorFromDictFilter.Appendix
                            with TfIdfFilter.Appendix
                            with NormalizeVectorFilter.Appendix { 
                        
                        val selection = cat.parent
                        val confName = "conf9"
                        override val minOcc = minWordCount
                    },
                    NoTrainSetSelection
                )
                
                val c45TitleConcatAbstractLearner = BoostedC45LearnerNT(
                    new History() 
                            with AbstractTitleConcat
                            with CategorySelectionFilter.Appendix
                            with VectorFromDictFilter.Appendix
                            with NormalizeVectorFilter.Appendix
                            with OddsRatioFilter.Appendix { 
                        val selection = cat.parent
                        val confName = "conf9"
                        override val minOcc = minWordCount
                        val orThreshold = orTh
                        val numWorst = 0
                    },
                    PrioritizeMainClass(maxTrainSetSize),
                    numAdaBoostIterations
                )
                
                val svmTitleConcatAbstractLearner = SvmLearnerNT(
                    new History() 
                            with AbstractTitleConcat
                            with CategorySelectionFilter.Appendix
                            with VectorFromDictFilter.Appendix
                            with TfIdfFilter.Appendix
                            with NormalizeVectorFilter.Appendix { 
                        
                        val selection = cat.parent
                        val confName = "conf9"
                        override val minOcc = minWordCount
                    },
                    NoTrainSetSelection
                )
                
                val c45JournalLearner = BoostedC45LearnerNT(
                    new History() 
                            with JournalProjection
                            with CategorySelectionFilter.Appendix
                            with VectorFromDictFilter.Appendix
                            with NormalizeVectorFilter.Appendix
                            with OddsRatioFilter.Appendix { 
                        
                        val selection = cat.parent
                        val confName = "conf2"
                        override val minOcc = 1
                        val orThreshold = 300.0
                        val numWorst = 0
                    },
                    PrioritizeMainClass(maxTrainSetSize),
                    numAdaBoostIterations
                )
                
                val c45KeywordLearner = BoostedC45LearnerNT(
                    new History() 
                            with TermsProjection
                            with CategorySelectionFilter.Appendix
                            with VectorFromDictFilter.Appendix
                            with NormalizeVectorFilter.Appendix
                            with OddsRatioFilter.Appendix { 
                        
                        val selection = cat.parent
                        val confName = "conf2"
                        override val minOcc = 1
                        val orThreshold = 300.0
                        val numWorst = 0
                    },
                    PrioritizeMainClass(maxTrainSetSize),
                    numAdaBoostIterations
                )
                
                val svmJournalLearner = SvmLearnerNT(
                    new History() 
                            with JournalProjection
                            with CategorySelectionFilter.Appendix
                            with VectorFromDictFilter.Appendix
                            with OddsRatioFilter.Appendix { 
                        val selection = cat.parent
                        val confName = "conf2"
                        val orThreshold = 300.0
                        val numWorst = 0
                        override val minOcc = 1
                    },
                    NoTrainSetSelection
                )
                
                val svmKeywordsLearner = SvmLearnerNT(
                    new History() 
                            with TermsProjection
                            with CategorySelectionFilter.Appendix
                            with VectorFromDictFilter.Appendix
                            with OddsRatioFilter.Appendix { 
                        val selection = cat.parent
                        val confName = "conf2"
                        val orThreshold = 300.0
                        val numWorst = 0
                        override val minOcc = 1
                    },
                    NoTrainSetSelection
                )
                
                val svmAbstractFlatLearner = SvmLearner(
                    new History() 
                            with AbstractProjection
                            with VectorFromDictFilter.Appendix
                            with TfIdfFilter.Appendix
                            with NormalizeVectorFilter.Appendix { 
                        val confName = "conf9"
                        override val minOcc = minWordCount
                    },
                    NoTrainSetSelection
                )
                
                val jsvmAbstractLearner = SvmLightJniLearner(
                    new History() 
                            with AbstractProjection
                            with CategorySelectionFilter.Appendix
                            with VectorFromDictFilter.Appendix
                            with TfIdfFilter.Appendix
                            with NormalizeVectorFilter.Appendix { 
                        
                        val selection = cat.parent
                        val confName = "conf9"
                        override val minOcc = minWordCount
                    },
                    NoTrainSetSelection
                )
                
                val jsvmUniLearner = SvmLightJniLearner(
                    new History() 
                            with AbstractTitleConcat
                            with CategorySelectionFilter.Appendix
                            with VectorFromDictFilter.Appendix
                            with TfIdfFilter.Appendix
                            with NormalizeVectorFilter.Appendix { 
                        
                        val selection = cat.parent
                        val confName = "conf9"
                        override val minOcc = minWordCount
                    },
                    NoTrainSetSelection
                )
                
                jsvmAbstractLearner.classifications(trainSet, testSet, cat)
                
                // check for validity
                /*val classificationsOnTest = learnerList.map(l => l.classificationsIfCached(testSet, cat))
                if(classificationsOnTest.forall(_ != None)) {
                    val ids = classificationsOnTest.map(_.get.map(_.id))
                    
                    println(ids.map(_.size).mkString("\n"))
                    /*val classificationsValid = ids.forall(_.size == ids(0).size) && (ids.map(_.sortBy(c => c)).transpose.map(c => 
                        if(c.isEmpty) true
                        else c.forall(_ == c.head)
                    ).forall(b => b))
                    
                    if(!classificationsValid) {
                        inconsitentCategoryList += cat.filenameExtension
                    } else {
                        consitentCategoryList += cat.filenameExtension
                    }*/
                }*/
                
                // val learnerList = List(c45AbstractLearner, c45TitleLearner/*, c45JournalLearner, c45KeywordLearner*/)
                // val learnerNames = List("c45AbstractLearner", "c45TitleLearner", "c45JournalLearner", "c45KeywordLearner")
                
                // val learnerList = List(c45TitleConcatAbstractLearner/*, svmTitleConcatAbstractLearner, c45KeywordLearner, c45JournalLearner, svmJournalLearner, svmKeywordsLearner, c45AbstractLearner, c45TitleLearner, svmAbstractLearner, svmTitleLearner*/)
                // val learnerNames = List("c45TitleConcatAbstractLearner"/*, "svmTitleConcatAbstractLearner", "c45Keyword", "c45Journal", "svmJournalLearner", "svmKeywordsLearner", "c45Abstract", "c45Title", "svmAbstract", "svmTitle"*/)
                
                // val learnerList = List(svmJournalLearner, svmKeywordsLearner)
                // val learnerNames = List("svmJournalLearner", "svmKeywordsLearner")
                
                // val learnerList = List(c45TitleConcatAbstractLearner)
                // val learnerNames = List("c45TitleConcatAbstractLearner")
                
                // val learnerList = List(c45TitleConcatAbstractLearner)
                // val learnerNames = List("svmAbstractConcatTitle")
                
                // val learnerList = List(svmAbstractLearner)
                // val learnerNames = List("svmAbstractLearner")
                
                // val learnerList = List(svmAbstractFlatLearner)
                // val learnerNames = List("svmAbstractFlatLearner")
                
                val learnerList = List(jsvmAbstractLearner/*svmAbstractLearner, svmTitleLearner, svmJournalLearner, svmKeywordsLearner, c45AbstractLearner, c45TitleLearner, c45JournalLearner, c45KeywordLearner*/)
                val learnerNames = List("svmAbstractLearner", "svmTitleLearner", "svmJournalLearner", "svmKeywordsLearner", "c45AbstractLearner", "c45TitleLearner", "c45JournalLearner", "c45KeywordLearner")            
    			
    			// val learnerList = List(svmTitleConcatAbstractLearner, c45TitleConcatAbstractLearner)
    			// val learnerNames = List("svmTitleConcatAbstractLearner", "c45TitleConcatAbstractLearner")
    			
    			// val classificationOnTestSet = learnerList.map(learner => (learner.classifications(trainSet, testSet, cat)))
    			// val classificationOnTuningSet = learnerList.map(learner => (learner.classifications(trainSet, tuningSet, cat)))
    			
    			/*for((c, name) <- classificationOnTestSet zip learnerNames) {
    				c match {
    					case Some(c) => evaluationDataAccumulator(name, c)
    					case None => println("skipped " + name + " for " + cat.filenameExtension)
    				}
    				// evaluationDataAccumulator(name, c)
    			}
    			
    			val normalizedAndThresholdedResults = (for((cTuning, cTest) <- classificationOnTuningSet zip classificationOnTestSet) yield {
    				if(cTest.isDefined) {
    					val t = RawClassification.findBestThreshold(cTuning.get)
    					Some(Pair(
    						RawClassification.normalize(RawClassification.withThreshold(cTest.get, t)), 
    						RawClassification.normalize(RawClassification.withThreshold(cTuning.get, t))
    					))
    				} else {
    				    None
    				}
    			}).toList
    			
    			
    			val combinations = List((List(0, 1, 4, 5), "allFeaturesSVM")/*, (List(4, 5, 6, 7), "allFeaturesC45"), (List(0, 4), "combineAbstract"), (List(1, 5), "combineTitle"), (List(2, 6), "combineJournal"), (List(3, 7), "combineKeywords")*/)
    			for((combinationIds, combinationName) <- combinations) {
    				val results = normalizedAndThresholdedResults.zipWithIndex.filter(i => combinationIds.contains(i._2)).map(_._1)
    				
    				if(results.forall(_ != None)) {
    					val normalizedAndThresholdedTestResults = results.map(_.get._1)
    					val normalizedAndThresholdedTuningResults = results.map(_.get._2)
    					
    					val coofficients = CombineClassifiers.findBestCoofficients(normalizedAndThresholdedTuningResults)
    					val combinedResult = RawClassification.weightedSumWithCoofficients(normalizedAndThresholdedTestResults, coofficients)
    					evaluationDataAccumulator(combinationName, combinedResult)
    				} else {
    					println("\n\n" + cat.filenameExtension + " skipped for combination\n")
    				}
    			}*/
            }}*/
            
    		/*val topK = layer match {
    			case 1 => 10
    			case 2 => 50
    			case 3 => 200
    		}
    		val writer = new BufferedWriter(new FileWriter(new File("results")));
    		
            println("top " + topK)
            evaluationDataAccumulator.precRecGraphs(Some(topK), writer)
            
            println("avg all")
            evaluationDataAccumulator.precRecGraphs(None, writer)
    		
    		writer.close()*/
        } catch {
            case ex: Throwable => {
                ex.printStackTrace()
            }
        } finally {
            FileManager.quit
        }
    }
    
    def launchAsynchronous(jobData: Iterable[List[CategoryIs]])(job: CategoryIs => Unit) {
        val startTime = System.currentTimeMillis()
        
        val threads = (for((d, threadIndex) <- jobData.zipWithIndex) yield new Thread( new Runnable() {
            override def run() {
                var count = 0
                for(cat <- d) {
                    job(cat)
                    count += 1
                    println("Thread: " + threadIndex)
                    printTimeDiff(startTime)
                    printProgress(count, d.size)
                }
            }
        }))
        
        for(thread <- threads) {
            thread.start();
        }
        
        for(thread <- threads) {
            thread.join();
        }
    }
    
    def splitInIndependentGroups[T](consideredCategories: List[String], independentBy: String => T, numThreads: Int) = {
        val independentGroups = consideredCategories.groupBy(independentBy).toList.map(_._2)
        val sortedGroups = independentGroups.sortWith((g1, g2) => g1.size > g2.size)
        
        val devidedGroups = {
            val groups = for(i <- 0 until numThreads) yield new mutable.ListBuffer[List[String]]()
            
            for(group <- sortedGroups) {
                groups.minBy(_.size)  += group
            }
            groups.map(catList => catList.flatten.sortBy(c => c).toList).toList
        }
        devidedGroups
    }
    
    def findConsideredCategories(corpus: ArffJsonInstancesSource, categoryCondition: String => Boolean, categoryMapping: String => String, minOccurences: Int): List[String] = { 
        val categoryCount = new mutable.HashMap[String, Int]() {
            override def default(key: String) = 0
        } 
        
        for(inst <- corpus; cat <- inst.categories.filter(categoryCondition).map(categoryMapping).distinct) {
            categoryCount(cat) += 1
        }
        
        categoryCount.toList.filter(c => c._2 >= minOccurences).sortBy(_._1).map(_._1)
    }
    
    def findConsideredCategories(corpus: ArffJsonInstancesSource, layer: Int, minOccurences: Int): List[String] = {
        val (categoryCondition, categorySubstring, categoryMapping, independenceGrouping) = layer match {
            case 1 => (
                ((c: String) => true), 
                ((c: String) => c.substring(0, 2)), 
                ((c: String) => CategoryIs.top(c.substring(0, 2))),
                ((c: String) => c.substring(0, 2))
            )
            
            case 2 => (
                ((c: String) => c.substring(2, 3) != "-"), 
                ((c: String) => c.substring(0, 3)), 
                ((c: String) => CategoryIs.topAndMiddle(c.substring(0, 2), c.substring(2, 3))),
                ((c: String) => c.substring(0, 2))
            )
            
            case 3 => (
                ((c: String) => c.substring(2, 3) != "-" && c.substring(3, 5).toLowerCase != "xx"), 
                ((c: String) => c), 
                ((c: String) => CategoryIs.topMiddleAndLeave(c.substring(0, 2), c.substring(2, 3), c.substring(3, 5))),
                ((c: String) => c.substring(0, 3))
            )
        }
        
        findConsideredCategories(corpus, categoryCondition, categorySubstring, minOccurences)
    }
            
    def printProgress(curr: Int, total: Int) {
        val percent = ((curr.toDouble * 100) / total)
        println("progress: " + (math.round(percent * 100).toDouble/100) + "%")
    }
    
    def printTimeDiff(startTime: Long) {
        val currTime = System.currentTimeMillis() - startTime
        println("time: " + (currTime/3600000) + "h " + ((currTime/60000)%60) + "min " + ((currTime/1000)%60) + "sec")
    }
    
    def performance(results: Seq[RawClassification], alpha: Double = 1.0) = {
        val prec = Classifier.precision(results)
        val rec = Classifier.recall(results)
        val f = Classifier.fMeasure(results, alpha)
        
        def format(number: Double) = "%.4f".format(number).toString.replaceAllLiterally(",", ".")
        
        "(" + format(prec) + ", " + format(rec) + ", " + format(f) + ")"
    }
}

object CombineClassifiers {
    def findBestCoofficients(learners: List[Learner], trainSet: ArffJsonInstancesSource with ContentDescribable, tuningSet: ArffJsonInstancesSource with ContentDescribable, cat: CategoryIs): List[Double] = {
        val classifications = learners.map(l => RawClassification.toBreakEven(l.classifications(trainSet, tuningSet, cat)))
        
        findBestCoofficients(classifications)
    }
    
    def findBestCoofficients(tuningSetResults: List[List[RawClassification]]): List[Double] = {
        def cross[B](a: List[List[B]], b: List[List[B]]) = for(aa <- a; bb <- b) yield aa ++ bb
        
        def comb(i: Int) = {
            val x = common.Common.sequenceWithountRoundingErrors(0.0, 1.0, 0.1)
            List.make(i, x.map(a => List(a))).reduceLeft((a, b) => cross(a, b)).filter(x => {val sum = x.reduceLeft(_ + _); sum > 0.99 && sum < 1.01})
        }
        
        val (bestCoofficients, bestFmeasure) = (for(coofficients <- comb(tuningSetResults.size)) yield {
            val res = RawClassification.weightedSumWithCoofficients(tuningSetResults, coofficients)
            (coofficients, Classifier.fMeasure(res, 1.0, 0.0))
        }).filter(!_._2.isNaN).maxBy(_._2)
		println((bestCoofficients, bestFmeasure))
		
        
        bestCoofficients
    }
}

case class EvaluationData(forMacro: List[Double], forMicro: List[Pair[Int, Int]], breakEvenFMeasure: Double)

class EvaluationDataAccumulator() {
    val data = new mutable.HashMap[String, mutable.ListBuffer[EvaluationData]] {
        override def default(key: String) = new mutable.ListBuffer[EvaluationData]
    }
    var numDataPoints = 0
    
    def apply(key: String, results: List[RawClassification]) {
        val list = data.getOrElse(key, new mutable.ListBuffer[EvaluationData])
        list += EvaluationData(
            RawClassification.precRecGraphPoints(results).map(_._2).toList,
            RawClassification.microPrecRecGraphPoints(results).toList,
            Classifier.fMeasure(RawClassification.toBreakEven(results), 1.0)
        )
        data(key) = list
        
        numDataPoints += 1
    }
    
    def precRecGraphs(top: Option[Int] = None, writer: BufferedWriter) {
        val results = (for((key, data) <- data) yield {
            val reducedData = top match {
                case Some(top) => {
                	data.toList.filter(r => !r.breakEvenFMeasure.isNaN()).sortWith((d1, d2) => d1.breakEvenFMeasure > d2.breakEvenFMeasure).take(top)
                }
                case None => data.toList
            }
            
            val totalTruePositivesForEachPercent = reducedData.map(_.forMicro).toList.transpose.map(list => list.map(_._1).reduceLeft(_ + _))
            val totalFalsePositivesForEachPercent = reducedData.map(_.forMicro).toList.transpose.map(list => list.map(_._2).reduceLeft(_ + _))
            
            val micro = (totalTruePositivesForEachPercent zip totalFalsePositivesForEachPercent).map(p => p._1.toDouble / (p._1 + p._2)).map(d => d.toString.replace('.', ','))
            
            val macro = (reducedData.map(_.forMacro).toList.transpose.map(list => {
	            val l = list.map(d => if(d.isNaN()) 0.0 else d)
	            val sum = l.sum
	            l.sum / l.size
	        })).map(d => d.toString.replace('.', ','))
	        key -> (micro, macro)
        }).toMap
        
        val microResults = results.toList.map(kv => kv._1 -> kv._2._1)
        val macroResults = results.toList.map(kv => kv._1 -> kv._2._2)
        
        val microResultsNames = microResults.map(_._1)
        val microResultsValues = microResults.map(_._2)
		
        writer.write(microResultsNames.mkString("\t") + "\n\n")
        writer.write(microResultsValues.transpose.map(_.mkString("\t")).mkString("\n") + "\n\n")
        
        val macroResultsNames = macroResults.map(_._1)
        val macroResultsValues = macroResults.map(_._2)
        writer.write(macroResultsNames.mkString("\t") + "\n\n")
        writer.write(macroResultsValues.transpose.map(_.mkString("\t")).mkString("\n") + "\n\n")
    }
}

@serializable trait TitleProjection extends ProjectionFilter.Appendix { val projection = (0, "tit") }
@serializable trait AbstractProjection extends ProjectionFilter.Appendix { val projection = (1, "abs") }
@serializable trait AbstractTitleConcat extends ConcatFilter.Appendix { val concat = (List(0, 1), "at")}
@serializable trait JournalProjection extends ProjectionFilter.Appendix { val projection = (2, "jour") }
@serializable trait TermsProjection extends ProjectionFilter.Appendix { val projection = (3, "ter") }


object SvmLightJniLearner {
    def apply(_history: CategoryIs => List[FilterFactory]) = new Learner {
        @transient val history = _history
        
        def fileAppendix = 
            "jsvm_" +
            "all"
            //trainSetSelectionDef.filenameAppendix
        
        def targetHistory(targetClassDef: CategoryIs) = history(targetClassDef)
        
        def trainClassifier(inst: ArffJsonInstancesSource, targetClassDef: CategoryIs): Classifier = SvmLightJniClassifier(
            inst,
            targetClassDef, 
            Some(this)
        )
        
        def loadClassifier(fullFilename: String) = SvmLightJniClassifier.load(fullFilename, Some(this))
    }
}

object SvmLearner {
    def apply(_history: CategoryIs => List[FilterFactory], trainSetSelection: TrainSetSelectionDefinition) = new Learner with TrainSetSelection with Thresholding {
        val trainSetSelectionDef: TrainSetSelectionDefinition = trainSetSelection
        
        @transient val history = _history
        
        def fileAppendix = 
            "svm_" +
            "all"
            //trainSetSelectionDef.filenameAppendix
        
        def targetHistory(targetClassDef: CategoryIs) = history(targetClassDef)
        
        def trainClassifier(inst: ArffJsonInstancesSource, targetClassDef: CategoryIs): Classifier = new JoachimsSVMClassifier(
            Map("-v" -> List("0")),
            inst,
            targetClassDef, 
            this
        )
        
        def loadClassifier(fullFilename: String) = JoachimsSVMClassifier.load(fullFilename, Some(this))
    }
}

object SvmLearnerNT {
    def apply(_history: CategoryIs => List[FilterFactory], trainSetSelection: TrainSetSelectionDefinition) = new Learner with TrainSetSelection {
        val trainSetSelectionDef: TrainSetSelectionDefinition = trainSetSelection
        
        @transient val history = _history
        
        def fileAppendix = 
            "svm_" +
            "all"
            //trainSetSelectionDef.filenameAppendix
        
        def targetHistory(categoryIs: CategoryIs) = history(categoryIs)
        
        def trainClassifier(inst: ArffJsonInstancesSource, targetClassDef: CategoryIs): Classifier = new JoachimsSVMClassifier(
            Map("-v" -> List("0")),
            inst,
            targetClassDef, 
            this
        )
        
        def loadClassifier(fullFilename: String) = JoachimsSVMClassifier.load(fullFilename, Some(this))
    }
}

object BoostedC45Learner {
    def apply(history: CategoryIs => List[FilterFactory], trainSetSelection: TrainSetSelectionDefinition, numIterations: Int) = new Learner with TrainSetSelection with Thresholding {
        val trainSetSelectionDef: TrainSetSelectionDefinition = trainSetSelection
        
        def fileAppendix = 
            "c45-boost-" + numIterations + "_thd_" +
            "all"
            // trainSetSelectionDef.filenameAppendix
            
        def targetHistory(targetClassDef: CategoryIs) = history(targetClassDef)
        
        def trainClassifier(inst: ArffJsonInstancesSource, targetClassDef: CategoryIs): Classifier = WekaClassifier(
            inst, 
            targetClassDef, 
            Some(this),
            { () => {
                val ada = new AdaBoostM1()
                ada.setClassifier(new J48)
                ada.setNumIterations(numIterations)
                ada
            }}
        )
        
        def loadClassifier(fullFilename: String) = WekaClassifier.load(fullFilename, Some(this))
    }
}

object BoostedC45LearnerNT {
    def apply(history: CategoryIs => List[FilterFactory], trainSetSelection: TrainSetSelectionDefinition, numIterations: Int) = new Learner with TrainSetSelection {
        val trainSetSelectionDef: TrainSetSelectionDefinition = trainSetSelection
        
        def fileAppendix = 
            "c45-boost-" + numIterations + "_thd_" +
            "all"
            // trainSetSelectionDef.filenameAppendix
            
        def targetHistory(categoryIs: CategoryIs) = history(categoryIs)
        
        def trainClassifier(inst: ArffJsonInstancesSource, categoryIs: CategoryIs): Classifier = WekaClassifier(
            inst, 
            categoryIs, 
            Some(this),
            { () => {
                val ada = new AdaBoostM1()
                ada.setClassifier(new J48)
                ada.setNumIterations(numIterations)
                ada
            }}
        ) 
        
        def loadClassifier(fullFilename: String) = WekaClassifier.load(fullFilename, Some(this))
    }
}

object C45Learner {
    def apply(history: CategoryIs => List[FilterFactory], trainSetSelection: TrainSetSelectionDefinition) = new Learner with TrainSetSelection {
        val trainSetSelectionDef: TrainSetSelectionDefinition = trainSetSelection
        
         def fileAppendix = 
            "c45_" + 
            trainSetSelectionDef.filenameAppendix
            
        def targetHistory(categoryIs: CategoryIs) = history(categoryIs)
        
        def trainClassifier(inst: ArffJsonInstancesSource, targetClassDef: CategoryIs): Classifier = WekaClassifier(
            inst, 
            targetClassDef, 
            Some(this), 
            { () => new J48() }
        )
        
        def loadClassifier(fullFilename: String) = WekaClassifier.load(fullFilename, Some(this))
    }
}











