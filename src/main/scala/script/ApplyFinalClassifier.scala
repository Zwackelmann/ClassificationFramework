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
import classifier.TopClassIs
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
import classifier.TopAndMiddleClassIs
import classifier.FixedTrainSetSelection

object ApplyFinalClassifier {
    def main(args: Array[String]) {
        // waehle ein Test- und TrainingSet
        val testSet = ArffJsonInstancesSource("exp", ContentDescription.TestSet, List())
        val trainSet = ArffJsonInstancesSource("exp", ContentDescription.TrainSet, List())

        // die stdTopClasses sind die Topklassen auf die ich meine Experimente ausgefuehrt habe
        val stdTopClasses = List(TopClassIs("00"), TopClassIs("01"), TopClassIs("08")/*NAN*/, TopClassIs("12")/*NAN*/, TopClassIs("13"), TopClassIs("15"), TopClassIs("19")/*NAN*/, TopClassIs("22")/*NAN*/, TopClassIs("35"), TopClassIs("91"))
        // allTopClasses sind alle Topklassen...
        val allTopClasses = common.Common.topClasses.map(c => TopClassIs(c))
        
        // hier wird ein Lerner definiert, der Klassifikatoren fuer beliebige Kategorien erstellen kann
        val learner = BoostedC45Learner( // erstelle einen C4.5 Learner mit AdaBoost
            new History() // erstelle eine neue History, die von allen Dokumenten des Learners eingehalten werden soll 
                    with AbstractProjection // Alle Dokumente sollen auf Abstracts projiziert werden
                    with VectorFromDictFilter.Appendix // Alle Dokumente sollen in den Vektorraum ueberfuehrt werden
                    with NormalizeVectorFilter.Appendix // Die Dokumente sollen normalisiert werden
                    with OddsRatioFilter.Appendix { // Und mithilfe der OddsRatio auf fuer die Kategorie sinnvoll Attribute projiziert werden 
                // Hier steht genaueres zur Konfiguration der History
                val confName = "conf9"
                override val minOcc = 3
                val orThreshold = 5.0
                val numWorst = 0
            },
            FixedTrainSetSelection(Some(1000), Some(200)), // Aus dem gesamten Trainingsset werden aus Peformancegruenden nur 1000 Dokumente aus der Zielkategorie gewaehlt und je 200 aus jeder Nicht-Zielkategorie
            30 // Es werden 30 AdaBoost-Iterationen ausgefuehrt
        )
        
        // ab hier werden fuer alle Klassen nacheinander die Klassifikationen berechnet
        for(targetClass <- allTopClasses) {
            val classifications = learner.classifications(testSet, targetClass) // berechne die Klassifikationen fuer das gegebene TestSet fuer die gegebene Zielklasse
            val toBreakEvenClassifications = RawClassification.toBreakEven(classifications) // verschiebe den Schwellwert der Klassifikationen, sodass Precision und Recall etwa gleich gross sind
            
            println("topClass: %s, results: %s".format(targetClass.toString, performance(toBreakEvenClassifications).toString)) // gebe die Ergebnisse auf der Konsole aus
        }
    }
    
    // eine kleine convinience funktion zur ausgabe der performance eines Classifiers...
    def performance(results: Seq[RawClassification], alpha: Double = 1.0) = {
        val prec = Classifier.precision(results, 0)
        val rec = Classifier.recall(results, 0)
        val f = Classifier.fMeasure(results, alpha, 0)
        
        def format(number: Double) = "%.4f".format(number).toString.replaceAllLiterally(",", ".")
        
        "(" + format(prec) + ", " + format(rec) + ", " + format(f) + ")"
    }
}

@serializable trait TitleProjection extends ProjectionFilter.Appendix { val projection = (0, "tit") }
@serializable trait AbstractProjection extends ProjectionFilter.Appendix { val projection = (1, "abs") }
@serializable trait JournalProjection extends ProjectionFilter.Appendix { val projection = (2, "jour") }
@serializable trait TermsProjection extends ProjectionFilter.Appendix { val projection = (3, "ter") }

object SvmLearner {
    def apply(_history: TargetClassDefinition => List[FilterFactory], trainSetSelection: TrainSetSelectionDefinition) = new Learner with TrainSetSelection {
        val trainSetSelectionDef: TrainSetSelectionDefinition = trainSetSelection
        
        @transient val history = _history
        
        def fileAppendix = 
            "svm_" + 
            trainSetSelectionDef.filenameAppendix
        
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
    def apply(history: TargetClassDefinition => List[FilterFactory], trainSetSelection: TrainSetSelectionDefinition, numIterations: Int) = new Learner with TrainSetSelection {
        val trainSetSelectionDef: TrainSetSelectionDefinition = trainSetSelection
        
        def fileAppendix = 
            "c45-boost-" + numIterations + "_" + 
            trainSetSelectionDef.filenameAppendix
            
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
    def apply(history: TargetClassDefinition => List[FilterFactory], trainSetSelection: TrainSetSelectionDefinition) = new Learner with TrainSetSelection {
        val trainSetSelectionDef: TrainSetSelectionDefinition = trainSetSelection
        
         def fileAppendix = 
            "c45_" + 
            trainSetSelectionDef.filenameAppendix
            
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

object NaiveBayesLearner {
    def apply(history: TargetClassDefinition => List[FilterFactory], trainSetSelection: TrainSetSelectionDefinition) = new Learner with TrainSetSelection {
        val trainSetSelectionDef: TrainSetSelectionDefinition = trainSetSelection
        
        def fileAppendix = 
            "bay_" + 
            trainSetSelectionDef.filenameAppendix
            
        def targetHistory(targetClassDef: TargetClassDefinition) = history(targetClassDef)
        
        def trainClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): Classifier = new WekaClassifier(
            inst, 
            targetClassDef, 
            this
        ) {
            def classifierConfig() = new NaiveBayes()
        }
        
        def loadClassifier(file: File) = WekaClassifier.load(file)
    }
}

object BoostedNaiveBayesLearner {
    def apply(history: TargetClassDefinition => List[FilterFactory], trainSetSelection: TrainSetSelectionDefinition, numIterations: Int) = new Learner with TrainSetSelection {
        val trainSetSelectionDef: TrainSetSelectionDefinition = trainSetSelection
        
        def fileAppendix = 
            "bay-boost-" + numIterations + "_" + 
            trainSetSelectionDef.filenameAppendix
            
        def targetHistory(targetClassDef: TargetClassDefinition) = history(targetClassDef)
        
        def trainClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): Classifier = new WekaClassifier(
            inst, 
            targetClassDef, 
            this
        ) {
            def classifierConfig() = {
                val ada = new AdaBoostM1()
                ada.setClassifier(new NaiveBayes)
                ada.setNumIterations(numIterations)
                ada
            }
        }
        
        def loadClassifier(file: File) = WekaClassifier.load(file)
    }
}











