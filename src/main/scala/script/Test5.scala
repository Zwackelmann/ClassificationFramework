package script
import model.RawClassification
import java.io.File

import parser.ArffJsonInstancesSource
object Test5 {
    def main(args: Array[String]) {
        val corpus = ArffJsonInstancesSource(new File("data/arffJson/corpus.json"))
        val numCats = corpus.map(inst => (inst.categories.map(_.substring(0, 2)).distinct.map(c => c -> inst.categories.size))).flatten.toList.groupBy(_._1).map(x => {
            val y = x._2.map(_._2)
            x._1 -> y.sum.toDouble / y.size
        })
        
        println(numCats.mkString("\n"))
    }
}




























