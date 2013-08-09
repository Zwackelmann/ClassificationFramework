package script.experiment

import parser.ArffJsonInstancesSource
import common.Path
import common.TrainTuningTestSetSelection
import common.FileManager
import parser.History
import filter.SimpleTextToFeatureVectorFilter
import filter.TfIdfFilter
import filter.NormalizeVectorFilter
import classifier.BalancedTrainSetSelection
import classifier.CategoryIsMSC
import classifier.Classifier
import script.SvmLightJniLearner
import script.AbstractTitleConcat
import model.RawClassification

object InductiveBoostingByChoosingMissclassifiedDocuments {
    def main(args: Array[String]) = try {
        common.Path.rootFolder = "data_inductive_boosing"
        val corpus = ArffJsonInstancesSource(Path.rootFolder + "/arffJson/corpus_2010-2013_ti+aben+py+dt.json")
        
        val ((trainSet, tuningSet, testSet), c) = TrainTuningTestSetSelection.getSets(
            100, 
            "test", 
            corpus, 
            (0.6, 0.2, 0.2)
        )
        
        val num19All = corpus.count(d => d.categories.exists(_.substring(0,2) == "19"))
        val num19Train = trainSet.count(d => d.categories.exists(_.substring(0,2) == "19"))
        val num19Test = testSet.count(d => d.categories.exists(_.substring(0,2) == "19"))
        
        println(num19All)
        println(num19Train)
        println(num19Train.toDouble / num19All)
        /*
        
        val svmLearner = SvmLightJniLearner(
            new History() 
                        with AbstractTitleConcat
                        with SimpleTextToFeatureVectorFilter.Appendix
                        // with TfIdfFilter.Appendix {
                        with NormalizeVectorFilter.Appendix { 
                },
                BalancedTrainSetSelection(Some(10000))
        )
        
        val results = {
            val r = svmLearner.classifications(
                trainSet, 
                testSet, 
                CategoryIsMSC.top(19)
            )
        
            val t = RawClassification.findBestThreshold(r, 1.0)
            
            RawClassification.withThreshold(r, t)
        }
        
        println(Classifier.precision(results))
        println(Classifier.recall(results))
        println(Classifier.fMeasure(results, 1.0))
        */
    } finally {
        FileManager.quit
    }
}














