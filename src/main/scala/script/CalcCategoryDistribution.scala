package script

import scala.collection.mutable.HashMap
import java.io.File
import parser.ContentDescription
import parser.ArffJsonInstancesSource

object CalcCategoryDistribution {
    def main(args: Array[String]) {
        val catCounter = new HashMap[Int, Int]
        
        val trainset = ArffJsonInstancesSource("data/arffJson/final-train.json", ContentDescription("final", ContentDescription.TrainSet, List()))
        val testset = ArffJsonInstancesSource("data/arffJson/final-test.json", ContentDescription("final", ContentDescription.TestSet, List()))
        
        for(inst <- trainset; cat <- inst.categories.map(c => c.substring(0, 2).toInt)) {
            catCounter(cat) = catCounter.getOrElse(cat, 0) + 1
        }
        for(inst <- testset; cat <- inst.categories.map(c => c.substring(0, 2).toInt)) {
            catCounter(cat) = catCounter.getOrElse(cat, 0) + 1
        }
        
        println(catCounter.toList.sortBy(x => x._1).map(x => x._1 + "\t" + x._2).mkString("\n"))
        
        // println(trainset.numInstances + testset.numInstances)
    }
}