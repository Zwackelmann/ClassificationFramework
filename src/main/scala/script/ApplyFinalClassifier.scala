package script
import classifier.Learner
import parser.ArffJsonInstancesSource
import classifier.TargetClassDefinition
import common.Path.classifierPath
import classifier.Classifier
import external.JoachimsSVMClassifier
import classifier.WekaClassifier
import model.RawClassification
import weka.classifiers.bayes.NaiveBayes
import weka.classifiers.trees.J48
import parser.ArffJsonInstancesFile
import classifier.TopClassIs
import filter.NominalizeFilter
import filter.FlattenFilter
import filter.ProjectionFilter
import filter.VectorFromDictFilter
import java.io.File
import filter.TfIdfFilter
import filter.NormalizeVectorFilter
import external.GensimLsiFilter
import filter.OddsRatioFilter
import format.arff_json.InstancesMappings
import parser.ContentDescription
import classifier.TrainSetSelection
import format.arff_json.HistoryItem
import classifier.FinalLearner
import classifier.FinalLearner2
import weka.classifiers.meta.AdaBoostM1
import format.arff_json.ArffJsonInstance
import filter.VectorFromNGramTreeFilter
import common.Common.FileConversion._
import format.arff_json.ArffJsonHeader
import format.arff_json.SparseArffJsonInstance
import format.arff_json.DenseArffJsonInstance
import filter.ScalingOddsRatioFilter

object ApplyFinalClassifier {
    def main(args: Array[String]) {
        val testSet = new ArffJsonInstancesFile("exp", ContentDescription.TestSet, List())
        val trainSet = new ArffJsonInstancesFile("exp", ContentDescription.TrainSet, List())
        
        //val finalLearner = new FinalLearner2(List(
            // lsi + svm
            /*SvmLearner(
                new AbstractOnlyLsiHistory(500, "conf5", true, false),
                Pair(None, None)
            )*//*, 
            SvmLearner(
                new TitleOnlyLsiHistory(250, "conf6", true, false),
                Pair(Some(1000), Some(200))
            )*//*, 
            // or + svm
            SvmLearner(
                new AbstractOnlyOrHistory(1.0),
                Pair(Some(1000), Some(200))
            ), 
            SvmLearner(
                new TitleOnlyOrHistory(1.0),
                Pair(Some(1000), Some(200))
            ),*/
            // or + boosted c45
            /*val boostedC45Learner = BoostedC45Learner(
                new AbstractOnlyOrHistory(1.0, 2000, "conf6"),
                Pair(Some(1000), Some(200)),
                10
            )*//*, 
            BoostedC45Learner(
                new TitleOnlyOrHistory(1.0),
                Pair(Some(1000), Some(200)), 
                30
            ),
            BoostedC45Learner(
                new JournalOnlyOrHistory(1.0),
                Pair(Some(1000), Some(200)),
                30
            ),
            BoostedC45Learner(
                new TermsOnlyOrHistory(1.0),
                Pair(Some(1000), Some(200)),
                30
            )*/
        //))
        
        /*val source = ArffJsonInstancesSource(
            List(
                new DenseArffJsonInstance("", List(), List("my stuff has lots of commutative Noetherian local Rings. And also many finite many associated prime. we also analyzed the usual local cohomology functor and the usual local cohomology functor again.")),
                new DenseArffJsonInstance("", List(), List("neumann total quotient ring weak finite conductor rings are cool. complete regular local ring is awesome and i also like finite generated graded algebra very much. blah two finite generated module la la la la "))
            ),
            new ArffJsonHeader("", List(), List()),
            ContentDescription("", ContentDescription.TrainSet, List())
        )*/
        
        /*val ngramfilter = new VectorFromNGramTreeFilter.Conf1(
            new File("data/ngrams/13-XX").lines.map(line => line.split("\\s+").toList),
            HistoryItem("ng-13")
        )
        
        val mappedInst = trainset
             .applyFilter(new ProjectionFilter(List(1), HistoryItem("proj-abs")))
             .applyFilter(ngramfilter)
        
        println(mappedInst.take(1000).mkString("\n"))*/ 
            
        /*val ngramclassifier = BoostedC45Learner(
            new NGramHistory(new File("data/ngrams/13-XX"), Pair(1, "abs")),
            Pair(Some(1000), Some(200)),
            1
        )*/
        
        // ngramclassifier.classifications(testset, TopClassIs("13"))
        
        def performance(results: Seq[RawClassification]) = {
            val prec = Classifier.precision(results, 0)
            val rec = Classifier.recall(results, 0)
            val f = Classifier.fMeasure(results, 1.0, 0)
            
            def format(number: Double) = "%.4f".format(number).toString.replaceAllLiterally(",", ".")
            
            "(" + format(prec) + ", " + format(rec) + ", " + format(f) + ")"
        }
        
        
        /*println("local1")
        for(targetClass <- List(TopClassIs("15"), TopClassIs("35"))) {
            println("\n\ntopClass = " + targetClass)
            val svmLearner = SvmLearner(
                new AbstractOnlyLsiHistory(500, true, true, true),
                Pair(None, None)
            )
            
            val results = svmLearner.classifications(testSet, targetClass)
            
            println("\n\n\n")
            println("results minus: true, topClass = " + targetClass + ": " + performance(results))
            
            val svmLearner2 = SvmLearner(
                new AbstractOnlyLsiHistory(500, false, true, true),
                Pair(None, None)
            )
            
            val results2 = svmLearner2.classifications(testSet, targetClass)
            
            println("\n\n\n")
            println("results minus: false, topClass = " + targetClass + ": " + performance(results2))
        }*/
        
        
        /*println("tbdb1")
        for(targetClass <- List(TopClassIs("00"), TopClassIs("01"), TopClassIs("13"), TopClassIs("15"), TopClassIs("35"))) {
            println("\n\ntopClass = " + targetClass)
            val sorC45Learner = C45Learner(
                new AbstractOnlyScalingOrHistory(1.0, 1000, 1.0, false),
                Pair(Some(1000), Some(200))
            )
            
            val results = sorC45Learner.classifications(testSet, targetClass)
            
            println("\n\n")
            println("topClass = " + targetClass + ": " + performance(results))
            println("\n\n")
        }*/
        
        /*println("local2")
        for(targetClass <- List(TopClassIs("00"), TopClassIs("01"), TopClassIs("15"), TopClassIs("35"))) {
            println("\n\ntopClass = " + targetClass)
            val svmLearner = SvmLearner(
                new AbstractOnlyLsiHistory(500, "conf7", true, true),
                Pair(None, None)
            )
            
            val results = svmLearner.classifications(testSet, targetClass)
            
            println("\n\n\n")
            println("results conf7, topClass = " + targetClass + ": " + performance(results))
            
            val svmLearner2 = SvmLearner(
                new AbstractOnlyLsiHistory(500, "conf8", true, true),
                Pair(None, None)
            )
            
            val results2 = svmLearner2.classifications(testSet, targetClass)
            
            println("\n\n\n")
            println("results conf8, topClass = " + targetClass + ": " + performance(results2))
        }*/
        
        /*println("local3")
        for(targetClass <- common.Common.topClasses.map(TopClassIs(_))) {
            println("\n\ntopClass = " + targetClass)
            val svmLearner = SvmLearner(
                new AbstractOnlyLsiHistory(500, "conf7", true, true),
                Pair(None, None)
            )
            
            val results = svmLearner.classifications(testSet, targetClass)
            
            println("\n\n\n")
            println("results conf7, topClass = " + targetClass + ": " + performance(results))
            
            val svmLearner2 = SvmLearner(
                new AbstractOnlyLsiHistory(500, "conf8", true, true),
                Pair(None, None)
            )
            
            val results2 = svmLearner2.classifications(testSet, targetClass)
            
            println("\n\n\n")
            println("results conf8, topClass = " + targetClass + ": " + performance(results2))
        }*/
        
        /*println("tbdb2")
        for(targetClass <- List(TopClassIs("00"), TopClassIs("01"), TopClassIs("13"), TopClassIs("15"), TopClassIs("35"), TopClassIs("91"))) {
            println("\n\ntopClass = " + targetClass)
            val sorC45Learner = C45Learner(
                new AbstractOnlyScalingOrHistory(1.0, 1000, 1.0, true),
                Pair(Some(1000), Some(200))
            )
            
            val results = sorC45Learner.classifications(testSet, targetClass)
            
            println("\n\n")
            println("topClass = " + targetClass + ": " + performance(results))
            println("\n\n")
        }*/
        
        /*println("tbdb3")
        for(targetClass <- List(TopClassIs("00"), TopClassIs("01"), TopClassIs("13"), TopClassIs("15"), TopClassIs("35"), TopClassIs("91"))) {
            println("\n\ntopClass = " + targetClass)
            val sorC45Learner = C45Learner(
                new AbstractOnlyOrHistory(1.0, 1000, true),
                Pair(Some(1000), Some(200))
            )
            
            val results = sorC45Learner.classifications(testSet, targetClass)
            
            println("\n\n")
            println("topClass = " + targetClass + ": " + performance(results))
            println("\n\n")
        }*/
        
        println("tbdb4")
        for(targetClass <- List(TopClassIs("00"), TopClassIs("01"), TopClassIs("08"), TopClassIs("13"), TopClassIs("15"), TopClassIs("35"), TopClassIs("91"))) {
            println("\n\ntopClass = " + targetClass)
            val sorC45Learner = C45Learner(
                new AbstractOnlyScalingOrHistory(1.0, 1000, 1.0, false),
                Pair(Some(1000), Some(200))
            )
            
            val results = sorC45Learner.classifications(testSet, targetClass)
            
            println("\n\n")
            println("topClass = " + targetClass + ": " + performance(results))
            println("\n\n")
        }
        
        /*for(minCount <- List(3, 5, 10, 15, 25, 50, 100); targetClass <- List(TopClassIs("01"), TopClassIs("15"), TopClassIs("35"))) {
            println("\n\nstarted minCount = " + minCount + ", topClass = " + targetClass)
            val svmLearner = SvmLearner(
                //new AbstractOnlyLsiHistory(500, "conf6", minCount, true, true),
                new AbstractOnlyScalingOrHistory(1.0, 0, minCount),
                Pair(None, None)
            )
            
            val results = svmLearner.classifications(testSet, targetClass)
            
            println("\n\n\n")
            println("results for minCount = " + minCount + ", topClass = " + targetClass + ": " + performance(results))
        }*/
    }
}


class TitleOnlyLsiHistory(numLsiDims: Int, confName: String, tfIdf: Boolean = true, normalize: Boolean = false) extends LsiHistory((0, "tit"), numLsiDims, confName, tfIdf, normalize)
class AbstractOnlyLsiHistory(numLsiDims: Int, confName: String, tfIdf: Boolean = true, normalize: Boolean = false) extends LsiHistory((1, "abs"), numLsiDims, confName, tfIdf, normalize)
@serializable
class LsiHistory(val projection: Pair[Int, String], val numLsiDims: Int, val confName: String, val tfIdf: Boolean = true, val normalize: Boolean = false) extends (TargetClassDefinition => List[HistoryItem]) {
    def apply(targetClassDef: TargetClassDefinition) = List(
        List(ProjectionFilter(List(projection._1), projection._2)), 
        List(VectorFromDictFilter(confName)), 
        if(tfIdf) List(TfIdfFilter()) else List(), 
        if(normalize) List(NormalizeVectorFilter()) else List(),
        List(GensimLsiFilter(numLsiDims))
    ).flatten
}

class TitleOnlyOrHistory(orThreshold: Double, numWorst: Int, minus: Boolean, normalize: Boolean = false) extends OrHistory((0, "tit"), orThreshold, numWorst, minus, normalize)
class AbstractOnlyOrHistory(orThreshold: Double, numWorst: Int, minus: Boolean, normalize: Boolean = false) extends OrHistory((1, "abs"), orThreshold, numWorst, minus, normalize)
class JournalOnlyOrHistory(orThreshold: Double, numWorst: Int, minus: Boolean, normalize: Boolean = false) extends OrHistory((2, "jour"), orThreshold, numWorst, minus, normalize)
class TermsOnlyOrHistory(orThreshold: Double, numWorst: Int, minus: Boolean, normalize: Boolean = false) extends OrHistory((3, "ter"), orThreshold, numWorst, minus, normalize)
@serializable
class OrHistory(val projection: Pair[Int, String], val orThreshold: Double, numWorst: Int, val minus: Boolean, val normalize: Boolean = false) extends (TargetClassDefinition => List[HistoryItem]) {
    def apply(targetClassDef: TargetClassDefinition) = List(
        List(ProjectionFilter(List(projection._1), projection._2)), 
        List(VectorFromDictFilter.conf6Minus(minus)), 
        if(normalize) List(NormalizeVectorFilter()) else List(),
        List(OddsRatioFilter(targetClassDef, orThreshold, numWorst))
    ).flatten
}

class AbstractOnlyScalingOrHistory(orThreshold: Double, numWorst: Int, shift: Double, normalize: Boolean = false) extends ScalingOrHistory((1, "abs"), orThreshold, numWorst, shift, normalize)
@serializable
class ScalingOrHistory(val projection: Pair[Int, String], val orThreshold: Double, numWorst: Int, shift: Double, val normalize: Boolean = false) extends (TargetClassDefinition => List[HistoryItem]) {
    def apply(targetClassDef: TargetClassDefinition) = List(
        List(ProjectionFilter(List(projection._1), projection._2)), 
        List(VectorFromDictFilter("conf8")), 
        List(ScalingOddsRatioFilter(targetClassDef, orThreshold, numWorst, shift)),
        if(normalize) List(NormalizeVectorFilter()) else List()
    ).flatten
}

class NGramHistory(file: File, projection: Pair[Int, String]) extends (TargetClassDefinition => List[HistoryItem]) {
    def apply(targetClassDef: TargetClassDefinition) = List(
        List(ProjectionFilter(List(projection._1), projection._2)), 
        List(VectorFromNGramTreeFilter("conf1", file))
    ).flatten
}

class JournalOnlyFlattenedHistory(vectorConf: String = "conf1") extends FlattenedHistory((2, "jour"), vectorConf)
class KeywordOnlyFlattenedHistory(vectorConf: String = "conf1") extends FlattenedHistory((3, "kw"), vectorConf)
@serializable
class FlattenedHistory(val projection: Pair[Int, String], val vectorConf: String = "conf1") extends (TargetClassDefinition => List[HistoryItem]) {
    def apply(targetClassDef: TargetClassDefinition) = List(
        ProjectionFilter(List(projection._1), projection._2), 
        NominalizeFilter(vectorConf), 
        FlattenFilter()
    )
}


object SvmLearner {
    def apply(history: TargetClassDefinition => List[HistoryItem], trainSetSelection: Pair[Option[Int], Option[Int]]) = new Learner with TrainSetSelection {
        val numTargetInst = trainSetSelection._1
        val numOtherInst = trainSetSelection._2
        
        def fileAppendix = 
            "svm_tss-" + 
            (trainSetSelection._1 match {case Some(v) => v.toString case None => "all"}) + "-" + 
            (trainSetSelection._2 match {case Some(v) => v.toString case None => "all"})
        
        def targetHistory(targetClassDef: TargetClassDefinition) = history(targetClassDef)
        
        def trainClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): Classifier = new JoachimsSVMClassifier(
            Map("-v" -> List("0")),
            inst,
            targetClassDef, 
            this
        )
        
        def loadClassifier(file: File) = JoachimsSVMClassifier.load(file)
    }
}

object BoostedC45Learner {
    def apply(history: TargetClassDefinition => List[HistoryItem], trainSetSelection: Pair[Option[Int], Option[Int]], numIterations: Int) = new Learner with TrainSetSelection {
        val numTargetInst = trainSetSelection._1
        val numOtherInst = trainSetSelection._2
        
        def fileAppendix = 
            "c45-boost-" + numIterations + "-tss-" + 
            (trainSetSelection._1 match {case Some(v) => v.toString case None => "all"}) + "-" + 
            (trainSetSelection._2 match {case Some(v) => v.toString case None => "all"})
            
        def targetHistory(targetClassDef: TargetClassDefinition) = history(targetClassDef)
        
        def trainClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): Classifier = new WekaClassifier(
            inst, 
            targetClassDef, 
            this
        ) {
            def classifierConfig() = {
                val ada = new AdaBoostM1()
                ada.setClassifier(new J48)
                ada.setNumIterations(numIterations)
                ada
            }
        }
        
        def loadClassifier(file: File) = WekaClassifier.load(file)
    }
}

object C45Learner {
    def apply(history: TargetClassDefinition => List[HistoryItem], trainSetSelection: Pair[Option[Int], Option[Int]]) = new Learner with TrainSetSelection {
        val numTargetInst = trainSetSelection._1
        val numOtherInst = trainSetSelection._2
        
         def fileAppendix = 
            "c45_tss-" + 
            (trainSetSelection._1 match {case Some(v) => v.toString case None => "all"}) + "-" + 
            (trainSetSelection._2 match {case Some(v) => v.toString case None => "all"})
            
        def targetHistory(targetClassDef: TargetClassDefinition) = history(targetClassDef)
        
        def trainClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): Classifier = new WekaClassifier(
            inst, 
            targetClassDef, 
            this
        ) {
            def classifierConfig() = new J48()
        }
        
        def loadClassifier(file: File) = WekaClassifier.load(file)
    }
}

object FlatteningBayesLearner {
    def apply(history: TargetClassDefinition => List[HistoryItem]) = new Learner {
        def fileAppendix = "flat-bay"
        def targetHistory(targetClassDef: TargetClassDefinition) = history(targetClassDef)
        
        def trainClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = new WekaClassifier(
            inst, 
            targetClassDef, 
            this
        ) {
            def classifierConfig() = new NaiveBayes()
            
            def aggregateClassifications(fun: (List[Double]) => Double)(groupedClassifications: Map[String, List[RawClassification]]) = {
                groupedClassifications.values.toList.map(cl => new RawClassification(
                    cl(0).id, 
                    fun(cl.map(_.classification)),
                    cl(0).realValue
                ))
            }
            
            val aggregatedByMax = aggregateClassifications((l: List[Double]) => l.reduceLeft((a, b) => math.max(a, b))) _
            val aggregatedByAvg = aggregateClassifications((l: List[Double]) => l.reduceLeft((a, b) => a + b) / l.size) _
            
            override def calculateClassifications(inst: ArffJsonInstancesSource) = {
                val calculated = aggregatedByMax(
                    super.calculateClassifications(inst)
                        .toList
                        .groupBy(c => c.id)
                )
                
                val baseSource = new ArffJsonInstancesFile(inst.contentDescription.withHistory(List()))
                val allIdsWithClass = baseSource.map(i => (i.id, targetClassDef(i.mscClasses)))
                
                val calculatedIds = calculated.map(_.id)
                val missingIdsWithClass = allIdsWithClass.toList.filter(idClass => !calculatedIds.contains(idClass._1))
                val missingClassifications = missingIdsWithClass.map(idClass => new RawClassification(idClass._1, 0.0, if(idClass._2) 1.0 else -1.0))
                calculated ++ missingClassifications
            }
        }
        
        def loadClassifier(file: File) = WekaClassifier.load(file)
    }
}












