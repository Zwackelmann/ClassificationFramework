package filter

import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonHeader
import scala.collection.mutable.HashMap
import java.io.File
import format.arff_json.SparseArffJsonInstance
import java.io.FileReader
import parser.ArffJsonInstancesSource
import parser.ArffJsonInstancesSource
import parser.History
import classifier.CategoryIs

object MostFrequentTermsFilter {
    def apply(numFeatures: Int) = new FilterFactory with Loadable[MostFrequentTermsFilter] {
        def apply(trainBase: ArffJsonInstancesSource) = new MostFrequentTermsFilter(trainBase, numFeatures)
        val historyAppendix = "mf-" + numFeatures
    }
    
    @serializable
    trait Appendix extends History {
        val numMostFrequentFeatures: Int
        abstract override def apply(categoryIs: CategoryIs) = super.apply(categoryIs) :+ MostFrequentTermsFilter(numMostFrequentFeatures)
    }
}

@serializable
class MostFrequentTermsFilter(source: ArffJsonInstancesSource, val numFeatures: Int) extends GlobalFilter {
    val targetIds = {
        val tf = new HashMap[Int, Int] {
            override def default(key: Int) = 0
        }
        
        for(example <- source.iterator) {
            for((key, value) <- example.sparseData if value != 0) {
                tf(key) = tf(key) + 1
            }
        }
        
        tf.toList.sortWith((a, b) => a._2 > b._2).take(numFeatures).map(x => x._1)
    }
    
    def applyFilter(source: ArffJsonInstancesSource) = {
        source.project(
            targetIds
        )
    }
}





















