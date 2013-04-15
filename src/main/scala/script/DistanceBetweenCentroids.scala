package script
import filter.VectorFromDictFilter
import java.io.File
import filter.feature_scoreing.FeatureScoreing
import filter.feature_scoreing.OddsRatio
import external.JoachimsSVMLearnApplier
import external.JoachimsSVMClassifyApplier
import java.io.BufferedReader
import java.io.FileReader
import common.Common.FileConversion._
import model.RawClassification
import format.arff_json.ArffJsonHeader
import parser.ArffJsonInstancesSource
import format.arff_json.ArffJsonInstance
import scala.collection.mutable
import parser.ContentDescription
import scala.collection.mutable

object DistanceBetweeenCentroids {
    def main(args: Array[String]) {
        val inst = ArffJsonInstancesSource(
            "data/arffJson/final-test_projection-abstract_vector-from-dict-conf4_tf-idf_lsi-500.json", 
            ContentDescription("final", ContentDescription.TestSet, List())
        )
        
        def merge(a: Map[Int, Double], b: Map[Int, Double], fun: (Double, Double) => Double) = {
            (for(key <- a.keys ++ b.keys) yield {
                val aValue = a.getOrElse(key, 0.0)
                val bValue = b.getOrElse(key, 0.0)
                key -> fun(aValue, bValue)
            }).toMap
        }
        
        val map = new mutable.HashMap[String, Pair[Map[Int, Double], Int]] {
            override def default(key: String) = (Map[Int, Double](), 0)
        }
        
        for(i <- inst) {
            for(group <- i.categories.map(_.substring(0, 2)).distinct) {
                val mapItem = map(group)
                map(group) = Pair(merge(mapItem._1, i.sparseData, (a, b) => a + b), mapItem._2 + 1)
            }
        }
        
        val centroids = map.mapValues(m => m._1.mapValues(v => v / m._2)).toMap
        
        println("|     |" + 
            (for(centroidId <- centroids.keys.toList.sortBy(s => s)) yield {
                " %7d |".format(centroidId.toInt)
            }).mkString("")
        )
        
        for((centroidId, centroid) <- centroids.toList.sortBy(_._1)) {
            val diffs = centroids.mapValues(m => {
                val diff = merge(centroid, m, (a, b) => a - b)
                math.sqrt(diff.values.map(a => a*a).reduceLeft(_ + _))
            })
            val out = ("| %3d |" format centroidId.toInt) + 
                (for(centroidId <- centroids.keys.toList.sortBy(s => s)) yield {
                    " %7.4f |".format(diffs(centroidId))
                }).mkString("")
                
            println(out)
        }
    }
}




















