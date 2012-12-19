package script
import model.RawClassification
import java.io.File

object Test5 {
    def main(args: Array[String]) {
        val res = RawClassification.fromFile(new File("data/results/exp_proj-abs_vec-conf9-min-10_svm_tss-all-all_tg-00XXXX_exp-test_proj-abs_vec-conf9-min-10.json"))
        
        val trueNegatives = res.filter(r => r.trueNegative)
        println(trueNegatives.map(_.classification).reduce(_ + _) / trueNegatives.size)
        
        val falseNegatives = res.filter(r => r.falseNegative)
        println(falseNegatives.map(_.classification).reduce(_ + _) / falseNegatives.size)
    }
}




























