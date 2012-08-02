package filter.applier

import java.io.File
import common.ObjectToFile.{readObjectFromFile => loadFilter}
import weka.filters.Filter
import weka.core.Instances
import weka.core.converters.ArffSaver
import java.io.BufferedReader
import java.io.FileReader
import common.Common._
import format.arff_json.ArffJsonInstances
import java.io.BufferedWriter
import java.io.FileWriter
import parser.ArffJsonInstancesFile
import parser.ArffJsonInstancesSource


class FilterApplier(filterFile: File) {
    val filter = loadFilter(filterFile).asInstanceOf[Filter]
    
    def applyFilter(source: ArffJsonInstancesSource, outputFile: File) {
        val instances = {
            val arffJsonInstances = new ArffJsonInstances(source, List())
            val newInstances = _applyFilter(arffJsonInstances.instances)
            arffJsonInstances.instances = newInstances
            
            val out = new BufferedWriter(new FileWriter(outputFile))
            arffJsonInstances.write(out, true)
            out.close
        }
    }
    
    
    private def _applyFilter(instances: Instances) = {
        for (i <- 0 until instances.numInstances()) {
            filter.input(instances.instance(i))
        }
        
        val newInstances = filter.getOutputFormat()
        
        def process(filter: Filter, newInstances: Instances) {
            val processed = filter.output
            if(processed != null) {
                newInstances.add(processed)
                process(filter, newInstances)
            }
        }
        process(filter, newInstances)
        newInstances
    }
}