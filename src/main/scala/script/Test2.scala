package script
import model.RawClassification
import java.io.File
import classifier.Classifier

object Test2 {
    def main(args: Array[String]) {
        def x(filename: String) = {
            val raw = RawClassification.fromFile(new File("data/results/" + filename))
            val normalized = try {
                RawClassification.normalize(raw)
            } catch {
                case e => raw
            }
            val weight = Classifier.precision(normalized) * 0.5 + Classifier.recall(normalized) * 0.5
            (raw, normalized, if(weight.isNaN()) 0 else weight)
        }
        
        for(topClass <- common.Common.topClasses if topClass.toInt <= 16) {
            println("topClass: " + topClass)
            
            val classificationsFinal = x("final_top-class-is-" + topClass + "_final-test.json")
            val classificationsAbstractLsiSvm = x("abstract-only-lsi_-500-svm_top-class-is-" + topClass + "_final-test.json")
            val classificationsAbstractOrSvm = x("abstract-only-odds-ratio_-2000-svm_top-class-is-" + topClass + "_final-test.json")
            val classificationsJournalBayes = x("journal-only-nominalized-flattened-bayes_top-class-is-" + topClass + "_final-test.json")
            val classificationsTermsBayes = x("terms-only-nominalized-flattened-bayes_top-class-is-" + topClass + "_final-test.json")
            val classificationsTitleLsiSvm = x("title-only-lsi_-100-svm_top-class-is-" + topClass + "_final-test.json")
            val classificationsTitleOrSvm = x("title-only-odds-ratio-2000-svm_top-class-is-" + topClass + "_final-test.json")
            
            val l = List(
                classificationsAbstractLsiSvm,
                classificationsAbstractOrSvm,
                classificationsJournalBayes,
                classificationsTermsBayes,
                classificationsTitleLsiSvm,
                classificationsTitleOrSvm
            )
            
            val weights = l.map(_._3)
            val totalWeight = weights.reduceLeft(_ + _) / weights.size
            
            val aggregatedClassifications = l
                .map(_._2.sortBy(c => c.id))
                .transpose
                .map(c => {
                    require(c.forall(_.id == c(0).id), "Inconsistent classifications to aggregate")
                    val cw = c.zip(weights)
                    val aggregatedClassification = (0.0 /: cw)((old, cw) => old + (cw._1.classification * cw._2)) / totalWeight
                    
                    new RawClassification(c(0).id, aggregatedClassification, c(0).realValue)
                })
                
            // println("before: ")
            // Classifier.report(classificationsFinal._1)
            // println("\nafter: ")
            Classifier.report(RawClassification.normalize(aggregatedClassifications))
            println("\n")
        }
    }
}








