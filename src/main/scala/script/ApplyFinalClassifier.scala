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

object ApplyFinalClassifier {
    def main(args: Array[String]) {
        /*val finalGen = new FinalClassifierGenerator(
            List(
                /*AbstractOnlyOddsRatioC45Generator,
                TitleOnlyOddsRatioSVMGenerator,*/
                AbstractOnlyLsiSVMGenerator//,
                /*AbstractOnlyOddsRatioSVMGenerator,
                JournalOnlyBayesGenerator,
                TermsOnlyBayesGenerator*/
            )
        )*/
        
        val testset = new ArffJsonInstancesFile("final", ContentDescription.TestSet, List())
        val trainset = new ArffJsonInstancesFile("final", ContentDescription.TrainSet, List())
        
        // val filter = SelectionFilter(TopClassIs("15"), None, None)(trainset)
        
        // filter.applyFilter(testset, TopClassIs("15")).save()
        // filter.applyFilter(trainset, TopClassIs("15")).save()
        
        val lsiAbstractSvmLearner = SvmLearner(
            new AbstractOnlyLsiHistory(500, "conf5", true, false),
            Pair(Some(1000), Some(200))
        )
        val lsiTitleSvmLearner = SvmLearner(
            new TitleOnlyLsiHistory(500, "conf5", true, false),
            Pair(Some(1000), Some(200))
        )
        val orAbstractC45Learner = C45Learner(
            new AbstractOnlyOrHistory(2000, "conf5", false),
            Pair(Some(1000), Some(200))
        )
        val orTitleC45Learner = C45Learner(
            new TitleOnlyOrHistory(2000, "conf5", false),
            Pair(Some(1000), Some(200))
        )
        val flatteningBayesJournalLearner = FlatteningBayesLearner(
            new JournalOnlyFlattenedHistory("conf1")
        )
        val flatteningBayesKeywordLearner = FlatteningBayesLearner(
            new KeywordOnlyFlattenedHistory("conf1")
        )
        
        val boostedOrAbstractC45Learner = BoostedC45Learner(
            new AbstractOnlyOrHistory(2500, "conf5", false),
            Pair(Some(1000), Some(200)),
            30
        )
        
        boostedOrAbstractC45Learner.classifications(testset, TopClassIs("15"))
        
        val finalLearner = new FinalLearner2(List(
            lsiAbstractSvmLearner,
            lsiTitleSvmLearner,
            orAbstractC45Learner,
            orTitleC45Learner,
            flatteningBayesJournalLearner,
            flatteningBayesKeywordLearner
        ))
        
        // finalLearner.calculateClassifications(testset, TopClassIs("15"))
        
        
        
        /*for(topClass <- common.Common.topClasses if topClass == "15") {
            println("\n\nStart evaluating results for topClass: " + topClass)
            
            lsiSvmLearner.report(testset, TopClassIs(topClass), 0, 0)
        }*/
    }
}


class TitleOnlyLsiHistory(numLsiDims: Int, vectorConf: String = "conf4", tfIdf: Boolean = true, normalize: Boolean = false) extends LsiHistory((0, "tit"), numLsiDims, vectorConf, tfIdf, normalize)
class AbstractOnlyLsiHistory(numLsiDims: Int, vectorConf: String = "conf4", tfIdf: Boolean = true, normalize: Boolean = false) extends LsiHistory((1, "abs"), numLsiDims, vectorConf, tfIdf, normalize)
@serializable
class LsiHistory(val projection: Pair[Int, String], val numLsiDims: Int, val vectorConf: String = "conf4", val tfIdf: Boolean = true, val normalize: Boolean = false) extends (TargetClassDefinition => List[HistoryItem]) {
    def apply(targetClassDef: TargetClassDefinition) = List(
        List(ProjectionFilter(List(projection._1), projection._2)), 
        List(VectorFromDictFilter(vectorConf)), 
        if(tfIdf) List(TfIdfFilter()) else List(), 
        if(normalize) List(NormalizeVectorFilter()) else List(),
        List(GensimLsiFilter(numLsiDims))
    ).flatten
}

class TitleOnlyOrHistory(numOrDims: Int, vectorConf: String = "conf4", normalize: Boolean = false) extends OrHistory((0, "tit"), numOrDims, vectorConf, normalize)
class AbstractOnlyOrHistory(numOrDims: Int, vectorConf: String = "conf4", normalize: Boolean = false) extends OrHistory((1, "abs"), numOrDims, vectorConf, normalize)
@serializable
class OrHistory(val projection: Pair[Int, String], val numOrDims: Int, val vectorConf: String = "conf4", val normalize: Boolean = false) extends (TargetClassDefinition => List[HistoryItem]) {
    def apply(targetClassDef: TargetClassDefinition) = List(
        List(ProjectionFilter(List(projection._1), projection._2)), 
        List(VectorFromDictFilter(vectorConf)), 
        if(normalize) List(NormalizeVectorFilter()) else List(),
        List(OddsRatioFilter(targetClassDef, numOrDims))
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
                
                // at this point there are only classifications calculated for all instances that contained at least one list item
                // if the list where empty the classification will be the average classification of all given classifications... or just 0??
                val defaultClassification = calculated.map(_.classification).reduceLeft(_ + _) / calculated.size
                
                val allIdsWithClass = inst.map(i => (i.id, targetClassDef(i.mscClasses)))
                println("all: " + allIdsWithClass.size)
                val calculatedIds = calculated.map(_.id)
                val missingIdsWithClass = allIdsWithClass.toList.filter(idClass => !calculatedIds.contains(idClass._1))
                println("calculated: " + calculated.size)
                println("missing: " + missingIdsWithClass.size)
                val missingClassifications = missingIdsWithClass.map(idClass => new RawClassification(idClass._1, 0.0, if(idClass._2) 1.0 else -1.0))
                calculated ++ missingClassifications
            }
        }
        
        def loadClassifier(file: File) = WekaClassifier.load(file)
    }
}












