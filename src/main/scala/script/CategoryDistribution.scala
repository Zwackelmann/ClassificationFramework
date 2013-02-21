package script
import parser.ArffJsonInstancesSource
import java.io.File
import scala.collection.mutable

object CategoryDistribution {
    def main(args: Array[String]) {
        val corpus = ArffJsonInstancesSource(new File("data/arffJson/corpus.json"))
        
        val countPerCat = {
            val map = new mutable.HashMap[String, Int] {
                override def default(key: String) = 0
            }
            
            for(inst <- corpus; cat <- inst.categories.filter(c => true).map(c => c.substring(0, 2)).distinct) {
                if(inst.categories.size != 0) {
                    map(cat) += 1
                }
            }
            
            map.toMap
        }
        
        println(countPerCat.toList.sortWith((a1, a2) => a1._2 < a2._2).map(c => c._2 + "\t" + c._1).mkString("\n"))
    }
}