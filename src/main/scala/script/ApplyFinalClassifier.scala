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
import java.util.Scanner
import classifier.CategorizationHierarchy
import classifier.CategoryIsMsc
import filter.SimpleTextToFeatureVectorFilter
import classifier.CategoryIsMscMain
import classifier.CategoryIsMscSome
import classifier.CategoryIsMscOnly

object Halt {
    private val s = new Scanner(System.in)
    def apply(message: String = "") {
        println(message + " press Enter...")
        s.nextLine()
    }
}

object ApplyFinalClassifier {
    def main(args: Array[String]) {
        val arguments = {
            /// args.toList
            List("corpus.json", "prod", "1")
            // List("confidenceSpaceCorpus.json", "conf", "1")
        }
        if(arguments.size != 3) {
            println("Erstes Argument: Corpus dateiname data/arffJson\nZweites Argument name des Korpus\nDrittes Argument layer fuer die Klassifizierung")
            exit(1)
        } 
        try {
            common.Path.rootFolder = "data_filter_irrelevant_papers"
                
            println("calculate statistics...")
            val corpusFilename = arguments(0)
            
            val corpus = ArffJsonInstancesSource(Path.rootFolder + "/arffJson/" + corpusFilename)
            
            val ((trainSet, tuningSet, testSet), c) = TrainTuningTestSetSelection.getSets(100, arguments(1), corpus, (0.6, 0.2, 0.2))
            
            // val consideredCategories = c map (c => CategoryIsMSC(c))
            
            val layer = arguments(2).toInt
            val minOccurences = 100
            
            val startTime = System.currentTimeMillis()
            val evaluationDataAccumulator = new EvaluationDataAccumulator()
            
            def jsvmUniLearner(cat: CategoryIs with CategorizationHierarchy, layer: Int) = SvmLightJniLearner(
                new History() 
                        with AbstractTitleConcat
                        // with CategorySelectionFilter.Appendix
                        with SimpleTextToFeatureVectorFilter.Appendix
                        // with VectorFromDictFilter.Appendix
                        with TfIdfFilter.Appendix
                        with NormalizeVectorFilter.Appendix { 
                    
                    // val confName = "conf11"
                    // override val minOcc = 3
                    // val selection = cat.parent
                    /*override val minOcc = layer match {
                        case 1 => 3
                        case 2 | 3 => 1
                        case _ => throw new RuntimeException("layer must be between 1 and 3")
                    }*/
                },
                BalancedTrainSetSelection(Some(10000))
                //NoTrainSetSelection
            )
            
            /*val thirdLevelClasses = consideredCategories
            val secondLevelClasses = thirdLevelClasses.map(_.parent)
            val firstLevelClasses = secondLevelClasses.map(_.parent)*/
            
            val targetCategories = ( 
                // List("05", "11", "14", "16", "20", "30", "32", "34", "35", "45", "53", "60", "68")
                common.Common.topClasses
            ).map(c => CategoryIsMscSome.top(c))
            
            var avgFAccu = 0.0
            var count = 0
            
            val reportStrings = new StringBuffer()
            
            for((cat, i) <- targetCategories.zipWithIndex) {
                printProgress(i, targetCategories.size)
                
                val learner = jsvmUniLearner(cat, layer)
                val r = learner.classifications(trainSet, tuningSet, cat)
                
                val precRecPoints = RawClassification.precRecGraphPoints(r)
                val bestF = bestFmeasure(precRecPoints)
                avgFAccu += bestF
                count += 1
                
                val numTrainingEamples = corpus.count(doc => { val m = cat.matchesForTraining(doc.categories); (if(m.isDefined) m.get else false) } )
                
                val reportStr = "{" + 
                        "\"topClass\" : \"" + cat.topClass.get + "\", " +
                        "\"numTrainingExamples\" : " + numTrainingEamples + ", " + 
                        "\"bestF\" : " + bestF + ", " +  
                        "\"precRecGraphPoints\" : " + "[" + precRecPoints.map(p => p._2).mkString(",") + "]" +
                    "}\n"
                println(reportStr)
                
                reportStrings.append(reportStr)
            }
            println("all finished")
            
            val macroF = (avgFAccu / count)
            reportStrings.append("macroF: " + macroF)
            
            println(reportStrings)
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
                ((c: String) => CategoryIsMscSome.top(c.substring(0, 2))),
                ((c: String) => c.substring(0, 2))
            )
            
            case 2 => (
                ((c: String) => c.substring(2, 3) != "-"), 
                ((c: String) => c.substring(0, 3)), 
                ((c: String) => CategoryIsMscSome.topAndMiddle(c.substring(0, 2), c.substring(2, 3))),
                ((c: String) => c.substring(0, 2))
            )
            
            case 3 => (
                ((c: String) => c.substring(2, 3) != "-" && c.substring(3, 5).toLowerCase != "xx"), 
                ((c: String) => c), 
                ((c: String) => CategoryIsMscSome.topMiddleAndLeave(c.substring(0, 2), c.substring(2, 3), c.substring(3, 5))),
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
            (coofficients, Classifier.fMeasure(res, 1.0))
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
    def apply(_history: CategoryIs => List[FilterFactory], trainSetSelection: TrainSetSelectionDefinition) = new Learner with TrainSetSelection {
        @transient val history = _history
        val trainSetSelectionDef: TrainSetSelectionDefinition = trainSetSelection
        
        def fileAppendix = 
            "jsvm_" +
            trainSetSelectionDef.filenameAppendix
        
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
    def apply(_history: CategoryIs => List[FilterFactory], trainSetSelection: TrainSetSelectionDefinition) = new Learner with TrainSetSelection {
        val trainSetSelectionDef: TrainSetSelectionDefinition = trainSetSelection
        
        @transient val history = _history
        
        def fileAppendix = 
            "svm_" +
            trainSetSelectionDef.filenameAppendix
        
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

object BoostedC45Learner {
    def apply(history: CategoryIs => List[FilterFactory], trainSetSelection: TrainSetSelectionDefinition, numIterations: Int) = new Learner with TrainSetSelection with Thresholding {
        val trainSetSelectionDef: TrainSetSelectionDefinition = trainSetSelection
        
        def fileAppendix = 
            "c45-boost-" + numIterations + "_thd_" +
            trainSetSelectionDef.filenameAppendix
            
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
            trainSetSelectionDef.filenameAppendix
            
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











