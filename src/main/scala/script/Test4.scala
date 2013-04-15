package script
import java.io.File
import common.Common.FileConversion._
import filter.feature_scoreing.OddsRatio
import java.io.BufferedWriter
import java.io.FileWriter
import parser.ContentDescription
import model.RawClassification
import classifier.Classifier
import model.CertaintyToThresholdFunction
import parser.ArffJsonInstancesSource

object Test4 {
    def main(args: Array[String]) {
		val res = List(
			"data/results/min100_proj-abs_sel-000____vec-conf9-min-1_tf-idf_norm_svm_all_000B___min100-test_proj-abs_sel-000____vec-conf9-min-1_tf-idf_norm.json", 
			"data/results/min100_proj-tit_sel-000____vec-conf9-min-1_tf-idf_norm_svm_all_000B___min100-test_proj-tit_sel-000____vec-conf9-min-1_tf-idf_norm.json",
			"data/results/min100_proj-jour_sel-000____vec-conf2-min-1_or-300.0-0-000B___svm_all_000B___min100-test_proj-jour_sel-000____vec-conf2-min-1_or-300.0-0-000B__.json", 
			"data/results/min100_proj-ter_sel-000____vec-conf2-min-1_or-300.0-0-000B___svm_all_000B___min100-test_proj-ter_sel-000____vec-conf2-min-1_or-300.0-0-000B__.json"
		).map(filename => RawClassification.fromFile(filename))
		
		val res2 = res map (r => {
		    val t = RawClassification.findBestThreshold(r)
			RawClassification.normalize(RawClassification.withThreshold(r, t))
		})
		
		println(res2(2).take(200).mkString("\n"))
		
		val p = Classifier.performanceValues(res2(2))
		println(p.map(x => (x._1, x._2.size)))
		
		println(Classifier.precision(res2(2)))
		println(Classifier.recall(res2(2)))
		// println(CombineClassifiers.findBestCoofficients(res2))
		// val res = RawClassification.fromFile(new File("min100_proj-jour_sel-94____vec-conf2-min-1_or-300.0-0-94B___svm_all_94B___min100-tuning_proj-jour_sel-94____vec-conf2-min-1_or-300.0-0-94B__.json"))
        // println(RawClassification.precRecGraphPoints(res))
		// println(Classifier.fMeasure(res, 1.0))
    }
}









