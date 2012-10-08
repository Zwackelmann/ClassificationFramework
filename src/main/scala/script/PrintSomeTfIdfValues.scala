package script
import parser.ArffJsonInstancesFile2
import parser.ContentDescription
import java.io.File
import scala.collection.mutable.HashMap
import parser.ArffJsonInstancesSource
import format.arff_json.SparseArffJsonInstance

object PrintSomeTfIdfValues {
    def main(args: Array[String]) {
        val source = new ArffJsonInstancesFile2(new File("data/arffJson/exp-train_proj-abs.json"), ContentDescription("exp", ContentDescription.TrainSet, List()))
        
        /*val (tf, numDocuments) = {
            val tf = new HashMap[Int, Int] {
                override def default(key: Int) = 0
            }
            
            var count = 0
            for(example <- source.iterator) {
                for((key, value) <- example.sparseData if value != 0) {
                    tf(key) = tf(key) + 1
                }
                count = count + 1
            }
            
            (tf.toMap, count)
        }
        
        
            source.map(
                elemFun = elements => elements.map(inst => {
                    val data = for((key, value) <- inst.sparseData) yield {
                        if(value == 0) key -> 0.0 
                        else {
                            key -> (value * math.log((numDocuments + 0.5) / (tf.getOrElse(key, 0) + 0.5)))
                        }
                    }
                    
                    new SparseArffJsonInstance(inst.id, inst.mscClasses, data, inst.numAttributes())
                }),
                headerFun = header => header,
                historyAppendix = historyAppendix
            )*/
    }
}