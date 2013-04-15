package script
import parser.ContentDescription
import java.io.File
import filter.VectorFromNGramTreeFilter
import common.Common.FileConversion._
import parser.ArffJsonInstancesSource

object Explore4Grams {
    def main(args: Array[String]) {
        // 2886 Uebereinstimmungen nach Liste
        // 3592 gefunden (vermutlich weitere Uebereinstimmungen durch Wortstammreduktion)
        val trainSet = ArffJsonInstancesSource("data/arffJson/final-test_proj-abs.json")
        val filter = new VectorFromNGramTreeFilter.Conf1(new File("data/ngrams/13-XX").lines.map(line => line.split(" ").toList))
        
        println(trainSet.applyFilter(filter).map(inst => inst.sparseData.size).reduceLeft(_ + _))
    }
}