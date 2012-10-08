package script
import parser.ArffJsonInstancesFile2
import parser.ContentDescription
import java.io.File
import filter.VectorFromNGramTreeFilter
import format.arff_json.HistoryItem
import common.Common.FileConversion._

object Explore4Grams {
    def main(args: Array[String]) {
        // 2886 Uebereinstimmungen nach Liste
        // 3592 gefunden (vermutlich weitere Uebereinstimmungen durch Wortstammreduktion)
        val trainSet = new ArffJsonInstancesFile2(new File("data/arffJson/final-test_proj-abs.json"), ContentDescription("exp", ContentDescription.TrainSet, List()))
        val filter = new VectorFromNGramTreeFilter.Conf1(new File("data/ngrams/13-XX").lines.map(line => line.split(" ").toList), HistoryItem(""))
        
        println(trainSet.applyFilter(filter).map(inst => inst.sparseData.size).reduceLeft(_ + _))
    }
}