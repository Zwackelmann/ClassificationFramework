package filter

import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonHeader
import scala.collection.mutable.HashMap
import java.io.File
import java.io.FileReader
import parser.ArffJsonInstancesSource
import parser.ArffJsonInstancesSource
import parser.History
import classifier.CategoryIs

object MostFrequentTermsFilter {
    def apply(numFeatures: Int) = new FilterFactory with Loadable[MostFrequentTermsFilter] {
        def apply(trainBase: ArffJsonInstancesSource) = new MostFrequentTermsFilter(trainBase, numFeatures) {
            override val trainingParams = Filter.trainingParams(historyAppendix, trainBase)
        }
        val historyAppendix = "mf-" + numFeatures
    }
    
    trait Appendix extends History with Serializable {
        val numMostFrequentFeatures: Int
        abstract override def apply(categoryIs: CategoryIs) = super.apply(categoryIs) :+ MostFrequentTermsFilter(numMostFrequentFeatures)
    }
}

abstract class MostFrequentTermsFilter(source: ArffJsonInstancesSource, val numFeatures: Int) extends GlobalFilter with Serializable {
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
            targetIds.toSet
        )
    }
}





















