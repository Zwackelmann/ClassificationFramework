package script
import parser.ArffJsonInstancesSource
import classifier.TopClassIs
import scala.collection.mutable
import parser.ArffJsonInstancesFile2
import java.io.File
import parser.ContentDescription
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonHeader
import format.arff_json.HistoryItem

object ExperimentsSetSelection {
    def main(args: Array[String]) {
        val trainSet = new ArffJsonInstancesFile2(new File("data/arffJson/final-train.json"), ContentDescription("final", ContentDescription.TrainSet, List()))
        val testSet = new ArffJsonInstancesFile2(new File("data/arffJson/final-test.json"), ContentDescription("final", ContentDescription.TestSet, List()))
        
        val trainSetExp = new ArffJsonInstancesFile2(new File("data/arffJson/exp-train.json"), ContentDescription("exp", ContentDescription.TrainSet, List()))
        val testSetExp = new ArffJsonInstancesFile2(new File("data/arffJson/exp-test.json"), ContentDescription("exp", ContentDescription.TestSet, List()))
        
        val trainSetGroups = analyzeSet(trainSet)
        val trainSetExpGroups = analyzeSet(trainSetExp)
        
        for(key <- trainSetGroups.keys) {
            val a = trainSetGroups(key)
            val b = trainSetExpGroups(key)
            
            println(a + " - " + b + " - " + (a.toDouble / b))
        }
    }
    
    def reduceSize(source: ArffJsonInstancesSource, factor: Double) = {
        source.map((it: Iterator[ArffJsonInstance]) => it.flatMap( inst => 
                if(math.random <= factor) List(inst) else List()
            ), 
            (header: ArffJsonHeader) => header,
            HistoryItem("reduced")
        )
    }
    
    def analyzeSet(source: ArffJsonInstancesSource) = {
        val map = new mutable.HashMap[String, Int] {
            override def default(key: String) = 0
        }
        
        for(inst <- source) {
            val groups = inst.mscClasses.map(_.substring(0, 2)).distinct
            for(group <- groups) {
                map(group) = map(group) + 1
            }
        }
        
        map.toMap
    }
}





