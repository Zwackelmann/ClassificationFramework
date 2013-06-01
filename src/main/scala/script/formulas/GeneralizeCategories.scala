package script.formulas

import script.SvmLightJniLearner
import script.AbstractTitleConcat
import filter.TfIdfFilter
import filter.NormalizeVectorFilter
import filter.VectorFromDictFilter
import classifier.BalancedTrainSetSelection
import parser.History
import parser.ArffJsonInstancesSource
import common.TrainTuningTestSetSelection
import classifier.CategoryIs
import classifier.CategoryIsMSC
import model.RawClassification
import classifier.Classifier

object GeneralizeCategories {
    def main(args: Array[String]) {
        // avg cluster size: 3.018867924528302
        // confusion threshold: 0.55
        
        /*val clusters = List(
            List(0), 
            List(1), 
            List(3), 
            List(5), 
            List(6, 8, 12, 19, 26, 31, 43, 44, 45, 46, 49, 58, 70, 97), 
            List(11), 
            List(12, 19, 26,     31,     43, 44, 45, 46,             97), 
            List(12, 19, 26,     31,     43, 44, 45,             70, 97), 
            List(12, 19, 26, 28, 31,     43, 44, 45, 46,             97), 
            List(12, 19, 26,     31, 40, 43, 44, 45, 46,             97), 
            List(12, 19, 26,     31,     43, 44, 45,     51,         97), 
            List(12, 19, 26,     31,     43,             51, 52        ), 
            List(13, 58), 
            List(14), 
            List(15), 
            List(16), 
            List(17, 22, 58), 
            List(18, 55, 58), 
            List(19, 31, 43, 55, 58), 
            List(20), 
            List(26, 33, 44, 58), 
            List(30), 
            List(31, 32, 58), 
            List(34), 
            List(35), 
            List(37, 70), 
            List(41, 42, 46), 
            List(46, 47, 49), 
            List(39, 53, 55, 57, 58), 
            List(54), 
            List(58, 85, 86), 
            List(60), 
            List(62), 
            List(65), 
            List(68), 
            List(74), 
            List(76), 
            List(78), 
            List(80), 
            List(81), 
            List(82), 
            List(83), 
            List(90), 
            List(91), 
            List(92), 
            List(93), 
            List(94)
        )*/
        
        val clusters = List(
            List(0, 1, 3,    6,    11,     13, 14, 15,     17,             22, 26, 28, 30, 31, 32, 33, 34, 35, 37, 39,     41, 42, 43,     45, 46, 47, 49, 51, 52, 53, 54,     57, 58, 60, 62, 65, 68, 70,     78,             82,             90,     92, 93, 94    ), 
            List(   1, 3, 5, 6, 8, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 26, 28, 30, 31, 32, 33, 34, 35, 37, 39, 40, 41, 42, 43, 44, 45, 46, 47, 49, 51, 52, 53, 54, 55, 57, 58, 60, 62, 65, 68, 70, 74, 78,     80, 81, 82,     85, 86, 90, 91, 92, 93, 94, 97), 
            List(      3,          11,     13, 14, 15,                     22, 26, 28, 30,     32, 33, 34, 35, 37, 39,     41, 42,         45, 46, 47, 49, 51, 52, 53, 54,     57, 58, 60, 62, 65,     70,     78,         81, 82, 83, 85,     90,         93, 94    ), 
            List(      3,                      14, 15,                     22, 26, 28, 30,     32,     34, 35, 37, 39,     41, 42,         45, 46, 47, 49,     52,                 58, 60, 62, 65,     70, 74, 76, 78, 80,     82,         86, 90,     92, 93, 94    )
        )
        
        common.Path.rootFolder = "data_try_train_set_selection"
        val corpus = ArffJsonInstancesSource(common.Path.rootFolder + "/arffJson/corpus.json")
        
        val ((trainSet, tuningSet, testSet), c) = TrainTuningTestSetSelection.getSets(100, "prod", corpus, (0.7, 0.3, 0.0))
        val minOccurences = 100
        
        val learner = SvmLightJniLearner(
            new History() 
                with AbstractTitleConcat
                with VectorFromDictFilter.Appendix
                with TfIdfFilter.Appendix {
                val confName = "conf9"
            },
            BalancedTrainSetSelection(Some(10000))
        )
        
        for(cluster <- clusters) {
            val r = learner.classifications(trainSet, tuningSet, CategoryIs.oneOf(cluster.map(c => CategoryIsMSC.top(c))))
            val t = RawClassification.findBestThreshold(r, 1.0)
            val r2 = RawClassification.withThreshold(r, t)
            
            println()
            println(cluster)
            println("old f1 measure: " + Classifier.fMeasure(r, 1.0))
            println("threshold: " + t)
            println("new f1 measure: " + Classifier.fMeasure(r2, 1.0))
            println()
        }
    }
}














