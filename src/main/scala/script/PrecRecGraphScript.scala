package script
import model.RawClassification
import java.io.File

object PrecRecGraphScript {
    def main(args: Array[String]) {
        val classifications = RawClassification.fromFile("exp_proj-abs_vec-conf9-min-3_svm_tss-all-all_tg-00XXXX_exp-test_proj-abs_vec-conf9-min-3.json")
        
        val sorted = classifications.sortWith((c1, c2) => c1.classification > c2.classification)
        val numPositives = classifications.count(c => c.realValue >= 0)
        
        val l = (List[Int]() /: sorted)((old, elem) => {
            if(elem.realValue >= 0.0) {
                if(old.isEmpty) List(1)
                else (old.head + 1) :: old
            } else {
                if(old.isEmpty) List(0)
                else old.head :: old 
            } 
        }).reverse
        
        for(percentPoints <- (1 to 100)) yield {
            val targetNumDocs = (numPositives * percentPoints) / 100
            val docsToTake = l.takeWhile(_ <= targetNumDocs).size
            
            (targetNumDocs.toDouble / docsToTake)
        }
    }
}