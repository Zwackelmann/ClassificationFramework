package filter.generator

import java.io.File
import weka.core.Instances
import weka.filters.Filter
import common.FileMeta
import java.io.BufferedReader
import java.io.FileReader
import weka.filters.unsupervised.attribute.StringToWordVector
import weka.core.stemmers.SnowballStemmer
import weka.core.converters.ArffSaver
import common.ObjectToFile.{writeObjectToFile => saveFilter}
import format.arff_json.ArffJsonInstances
import common.Common._
import parser.ArffJsonInstancesFile
import parser.ArffJsonInstancesSource

object WekaFilterGenerator {
}

abstract class WekaFilterGenerator() {
    def filterConfig(): Filter
    
    def genFilter(source: ArffJsonInstancesSource, outputFile: File) {
        val instances = new ArffJsonInstances(source, List()).instances
        
        val filter = filterConfig()
        filter.setInputFormat(instances)
        
        for (i <- 0 until instances.numInstances()) {
            filter.input(instances.instance(i))
        }
        
        filter.batchFinished()
        filter.resetQueue()
        
        saveFilter(filter, outputFile)
    }
}




