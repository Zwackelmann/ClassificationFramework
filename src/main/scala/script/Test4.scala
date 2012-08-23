package script
import java.io.File
import common.Common.FileConversion._
import filter.feature_scoreing.OddsRatio
import parser.ArffJsonInstancesFile
import java.io.BufferedWriter
import java.io.FileWriter
import parser.ContentDescription
import parser.ArffJsonInstancesFile2
import model.RawClassification
import classifier.Classifier


object Test4 {
    def normalizeClassifications(classifications: Iterable[RawClassification]) = {
        val avgAbsClassification = classifications.map(c => math.abs(c.classification)).reduceLeft(_ + _) / classifications.size
        classifications.map(c => new RawClassification(c.id, c.classification / avgAbsClassification, c.realValue))
    }
    
    def main(args: Array[String]) {
        val res = List(
            RawClassification.fromFile(new File("data/results/final_proj-abs_vec-conf5_or-1500-tg-15XXXX_c45-boost-30-tss-1000-200_tg-15XXXX_final-test_proj-abs_vec-conf5_or-1500-tg-15XXXX.json"))
            //RawClassification.fromFile(new File("data/results/final_proj-tit_vec-conf5_or-2000-tg-15XXXX_c45_tss-1000-200_tg-15XXXX_final-test_proj-tit_vec-conf5_or-2000-tg-15XXXX.json")),
            //RawClassification.fromFile(new File("data/results/final_proj-tit_vec-conf5_tf-idf_lsi-500_svm_tss-1000-200_tg-15XXXX_final-test_proj-tit_vec-conf5_tf-idf_lsi-500.json")),
            //RawClassification.fromFile(new File("data/results/final_proj-abs_vec-conf5_tf-idf_lsi-500_svm_tss-1000-200_tg-15XXXX_final-test_proj-abs_vec-conf5_tf-idf_lsi-500.json"))
        )
        
        val combinations = (for(i <- 1 to res.length) yield {
            res.zipWithIndex.combinations(i)
        }).flatten
        
        for(combination <- combinations) {
            println(combination.map(_._2).mkString(", ") + ":")
            val resList = combination.map(_._1)
            
            val resultsAndWeights = resList.map(
                res => {
                    val results = normalizeClassifications(res)
                    val weight = {
                        val m = Classifier.fMeasure(res, 1.0)
                        if(m.isNaN()) 0 else m
                    }
                    (results, weight) 
                }
            )
            
            val results = resultsAndWeights.map(_._1)
            val weights = resultsAndWeights.map(_._2)
            val normedWeights = weights.map(w => w / weights.reduceLeft(_ + _))
            
            val finalRes = results.transpose.map(classificationList => {
                require(classificationList.forall(c => c.id == classificationList(0).id))
                new RawClassification(
                    classificationList(0).id, 
                    ((0.0 /: (classificationList zip normedWeights))(
                        (old, clw) => old + clw._1.classification * clw._2
                    )),
                    classificationList(0).realValue)
            })
            
            // RawClassification.save(finalRes, new File("data/results/final.json"))
            
            println("mine")
            print("%.4f".format(Classifier.precision(finalRes, 0)))
            print("\t")
            print("%.4f".format(Classifier.recall(finalRes, 0)))
            print("\t")
            print("%.4f".format(Classifier.fMeasure(finalRes, 1.0, 0)))
            
            println("\nadaBoost")
            val adaBoostRes = RawClassification.adaBoost(results)
            print("%.4f".format(Classifier.precision(adaBoostRes, 0)))
            print("\t")
            print("%.4f".format(Classifier.recall(adaBoostRes, 0)))
            print("\t")
            print("%.4f".format(Classifier.fMeasure(adaBoostRes, 1.0, 0)))
            
            println("\n")
        }
    }
}









